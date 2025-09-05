# Runix Agentic Framework - Architecture Design

## Current State
Runix is a task execution framework built on Haskell with:
- Polysemy effect system for controlled IO
- Safe Haskell security model  
- Type-safe LLM integration via LLMTypes
- Task composition and Nix-based builds
- 780-line foundation providing robust architecture

## Agentic Extension Vision
Extend Runix with **agentic capabilities** that address core problems in current AI assistant tools:

## Core Problems Being Solved

### 1. Context Limitations
**Problem**: Current AI assistants lose coherence on multi-file/project tasks
- **Solution**: Specialized sub-agents working on bounded, focused problems

### 2. Loss of Type Information
**Problem**: Current systems use text-only communication, losing semantic meaning
- **Solution**: Type-safe agent interfaces leveraging Haskell's type system while LLMs work with text internally

### 3. Poor Classical Code Integration
**Problem**: LLMs are terrible at tasks trivial for algorithms (sorting, parsing)
- **Solution**: Seamless hybrid computation - agents use appropriate tools (classical algorithms vs LLM reasoning) for each sub-problem

### 4. Coarse Safety Controls
**Problem**: Current AI safety is binary (allowed/not allowed)
- **Solution**: Fine-grained capability control through effect system - precise permissions, not binary access

## Key Design Principles

**Everything is a Function**: Whether pure code, effects, LLM calls, or autonomous agents - all compose as `f input -> output`

**Minimal Architecture**: Agents are just functions, tools are just functions, no special agent machinery required

**Effect-Controlled Safety**: All operations through Polysemy effects, enabling composable permission restrictions

**Type Safety at Boundaries**: LLM interface is the only unsafe boundary, everything else maintains type safety

## Success Vision
A framework enabling agents that are both **powerful** (autonomous reasoning + tool use) and **reliable** (type safety + effect control), bridging current AI flexibility with functional programming safety/composability.

## Agent Spectrum
Agents exist on a spectrum from general to highly specialized:

### General Agents
```haskell
generalAgent :: (LLMOutput result, Members '[LLM] r) 
  => Toolbox r -> Prompt -> Goal -> Context -> Sem r result
```
- Configurable prompt and toolbox
- Maximum flexibility for exploration

### Specialized Agents
```haskell
debugAgent :: Members '[LLM, FileSystem, CompileTask] r
  => BugReport -> Context -> Sem r Solution
```
- Fixed toolbox, domain-specific functionality
- More predictable, reliable behavior

### Highly Constrained Agents
```haskell
csvProcessor :: Members '[LLM, FileSystem] r 
  => CsvTask -> Sem r ProcessedCsv
```
- Fixed workflow, minimal configurability
- Maximum reliability and performance

## Core Architecture

### LLM Interface Design
Enhanced `LLMOutput` typeclass provides robust parsing with progressive error recovery:

```haskell
class LLMOutput a where
    fromLLMText :: Text -> Either ParseError a
    name :: Text                    -- "file path", "article summary"
    description :: Text             -- Full requirements and constraints
    format :: Text                 -- Technical format specification
    examples :: [Text]             -- Concrete examples for error recovery
```

**Progressive Context Strategy**:
- Initial prompts stay minimal (avoid example bias)
- Error recovery adds examples and detailed requirements only when needed
- Single retry policy for syntactic errors (semantic errors handled downstream)

### Tool System
Tools are just functions with effect constraints and metadata for LLM understanding:

```haskell
type Toolbox r = [Tool r]
data Tool r = Tool 
  { toolName :: Text                    -- "read_file"
  , toolDescription :: Text             -- "Read contents of a file from filesystem"
  , parameterDescription :: Text        -- "filepath: absolute path to file to read"
  , toolCall :: ToolParams -> Sem r ToolResult  
  }
```

**Key Insights**:
- No special tool registration needed - pass `Toolbox r` to agents
- Parameter validation handled by existing `LLMOutput` instances
- Tool permissions controlled by effect constraints (`Members [...] r`)
- Toolboxes are composable and context-specific
- Existing Runix tasks integrate seamlessly as tools

### Agent Execution
Agents are simple pattern-matching loops:

```haskell
agentStep :: (LLMOutput result, Members '[LLM] r) 
  => Toolbox r -> Goal -> Context -> Sem r (Either result Context)
```

**Core Loop**:
1. Query LLM with current context
2. Parse response (final result OR tool call)  
3. Execute tool call if needed, update context
4. Single retry for format errors, then fail
5. Recurse until final result

## Key Architectural Insights

### Type Safety Boundaries
**Fundamental Limitation**: Perfect type safety across LLM boundaries is impossible - LLMs can generate arbitrary text.

**Solution**: Contain type unsafety to LLM parsing boundary only:
- Syntactic validation: Our responsibility (retry-able)  
- Semantic validation: Downstream effects' responsibility (normal error handling)
- Type safety maintained everywhere except LLM text → Haskell value parsing

### Automatic Verification
Specialized agents can enforce mandatory verification steps:

```haskell
-- Coding agent with automatic compilation checking  
codingAgent :: Members '[LLM, FileSystem, CompileTask] r
  => CodeTask -> Context -> Sem r Solution
-- Automatically compiles after file modifications, feeds errors back to LLM
```

### Multi-Agent Coordination
Sub-agents spawn through normal function composition:
- No special agent orchestration machinery
- Context sharing through explicit parameter passing
- Permission inheritance via effect constraints

### Iterative Agent Execution Pattern
Agents control their own process flow through action loops:

```haskell
data AgentAction r = 
  CallTool Text [Value]  -- tool name + parameters
  | Finished Value       -- final result

agentLoop :: Members '[LLM] r => Context -> Sem r Result
agentLoop context = do
  action <- agent context
  case action of
    CallTool toolName params -> do
      result <- executeTool toolName params
      newContext <- updateContext context result
      agentLoop newContext
    Finished result -> return result
```

### Structural Verification Patterns
Verification is enforced through function composition, not LLM behavior:

```haskell
-- Agent cannot return successfully without verification
safeCodeWriter :: Members '[LLM, FileSystem, CompileTask] r => CodeTask -> Sem r VerifiedCode
safeCodeWriter task = agentLoop (initialContext task)
  where
    agentLoop context = do
      action <- codeAgent context
      case action of
        Finished code -> do
          result <- compileTask code
          case result of
            CompileSuccess _ -> return $ VerifiedCode code
            CompileFail errors -> agentLoop (addErrors context errors)
        CallTool name params -> do
          result <- executeTool name params
          agentLoop (updateContext context result)
```

**Key Insight**: Agents get full iterative control while verification is architecturally guaranteed.

## Implementation Assessment

### Simplicity Validation
The architecture analysis confirms this is **framework extension, not transformation**:
- Agents are just functions with LLM calls and tool execution
- Tools are just functions with effect constraints  
- No special agent effects or orchestration machinery needed
- ~100 lines of additional code on 780-line foundation

### Effect System Advantages
Complex features emerge naturally from compositional effects:
- Multi-agent orchestration: Function composition
- Resource constraints: Effect interpreters with budgets  
- Permission control: Effect stack manipulation
- Observability: Logging effect with context

### Strategic Benefits
- **MCP Integration**: Export Runix tools via Model Context Protocol for ecosystem connectivity
- **Deployment Flexibility**: CLI, chat, REST API, MCP server - all use same core
- **Competitive Differentiation**: Only framework combining functional programming + type safety + effect systems for agents
- **Market Position**: Address 73% enterprise AI project failure rate through reliability
- **Semantic Service Abstraction**: Typeclass-based service categories enable write-once, run-anywhere agents across service ecosystems
- **Compositional Verification**: Function composition enforces structural guarantees that LLMs cannot bypass

### Success Criteria
- Agents complete multi-step workflows autonomously
- Type safety preserved (except at LLM boundary)
- Performance suitable for production deployment
- Intuitive developer experience for Haskell programmers  
- Clear migration path from existing Runix tasks

## Implementation Details Resolved

### Context Management
**Solution**: Composable middleware system in LLM effect interpreters:
```haskell
smartContextManager = 
  withContextCompaction . 
  withToolCallPriority . 
  withContextTruncation 4000
```
- Provider-independent truncation/compaction strategies
- Mix and match approaches for different use cases
- Keeps agent code clean of context management complexity

### Error Handling
**Strategy**: Use `Fail` for genuine failures, `Either` for recoverable parsing:
- LLM refuses task, network failures → `Fail` (execution stops)
- Parse errors, tool parameter format issues → `Either` (retry with context)
- Simple and predictable for developers

### Context Serialization
**Approach**: Single internal format, provider-specific conversion:
- Choose simple internal representation (e.g., `[Message]`)
- Each LLM provider interpreter converts to their specific format
- Implementation detail, not architectural concern

## LLM-Generated Agent Development

### Agent Code Generation Strategy
The functional, compositional architecture enables LLM generation of agent code:

**Approach**: Pseudocode → Haskell transformation rather than DSL abstraction
```haskell
-- LLM writes planning pseudocode, then transforms to Haskell
{-
TASK: Fix compilation errors in Haskell project
1. Read the main source file
2. Try to compile the project  
3. If compilation fails:
   - Analyze error messages
   - Apply fixes
   - Retry compilation
4. Return success when compilation passes
-}

-- Transform to implementation
fixCompilationErrors :: Members '[FileSystem, CompileTask, LLM] r => Project -> Sem r FixResult
```

**Key Enablers**:
- **Type signatures as documentation**: Effect constraints guide implementation and provide compiler verification
- **Tool-driven development**: LLM lists needed tools, effect constraints follow naturally from tool selection
- **Compositional patterns**: Clear templates for agent loops, verification wrappers, and hierarchical delegation
- **Compiler feedback**: Type errors guide LLM toward correct implementations

### Toolbox-First Development
```haskell
-- LLM specifies desired tools
toolbox = [readFileTool, writeFileTool, compileTaskTool, sendMessageTool]
-- Effect constraints inferred from toolbox
-- Members '[FileSystem, CompileTask, RestAPI Slack] r
```

**Confidence Assessment**: High feasibility for LLM generation of agents, verification wrappers, and workflow composition due to architectural regularity and strong type guidance.

## Implementation Readiness
- **Technical architecture**: Complete and validated
- **All design questions resolved**: Context management, error handling, tool system, integration patterns, verification patterns
- **Implementation scope**: Minimal (~100 lines core + composition helpers)
- **Risk assessment**: Low technical risk
- **LLM Generation**: Architecture designed to support automated agent development
- **Next step**: Begin implementation with enhanced LLM interface and basic agent loop