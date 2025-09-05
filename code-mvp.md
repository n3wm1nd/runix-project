# Runix Claude Code MVP - Implementation Plan

## Core Architecture Foundation

Building on the existing 780-line Runix foundation with Polysemy effects system to create a Claude Code-equivalent coding assistant.

## Essential Infrastructure Components

### 1. API Integrations
- **Anthropic API**: Native tool calling support, optimal for coding assistance
- **OpenAI API**: De facto standard, provides compatibility with OpenRouter, local models (Ollama), and broader ecosystem

### 2. Effect System Extensions

#### AiFileSystem Effect
Smart file operations for AI contexts:
- Size limits for reasonable AI processing
- Change detection (only read files if modified)
- Gitignore awareness filtering
- Notify LLM of file changes without reading full content
- Context-efficient operations

Keep existing `FileSystem` effect for classical code where file size doesn't matter.

#### Observable Effect
Real-time user updates without blocking returns:
- Non-blocking status updates to interface
- Structured data logging for UI consumption
- Progress indicators for long operations

#### Rate/Cost Limiting
Effect-level budget tracking:
- API usage monitoring
- Cost limit enforcement
- Agent awareness of budget constraints
- Graceful degradation when limits approached

### 3. Streaming Infrastructure
- **Conduits integration**: Stream responses progressively
- **Sync programming model**: Overall control flow remains blocking
- **Real-time updates**: Users see progress via Observable effect instead of wondering if frozen

### 4. Core Tool Set (Claude Code Feature Parity)
- File operations (read, write, search, glob patterns) via AiFileSystem
- Command execution with output streaming
- Code analysis (compilation, type checking, linting) via existing CompileTask
- Multi-tool orchestration

#### Smart Tool Behavior
- **Automatic verification**: Code modification tools automatically run analysis after changes
- **No arbitrary command execution**: Constrained to safe, necessary operations
- **Context-aware operations**: Tools understand project structure and constraints

### 5. CLI Chat Interface
- Interactive conversation loop
- Streaming response display
- Status updates and progress indicators
- Error handling with helpful messages

## Architectural Advantages

### Built-in Improvements Over Claude Code
- **Automatic verification**: Code changes trigger immediate analysis
- **Effect-based security**: Fine-grained permission control instead of binary access
- **Type safety boundaries**: Clear separation between safe/unsafe operations
- **Smart context management**: Only process files that matter

### Extensibility Foundation
- **Tool system**: Easy to add new capabilities
- **Effect composition**: Complex behaviors emerge from simple effect combinations
- **Multi-API support**: Provider-agnostic agent implementation

## Implementation Strategy

### Phase 1: Infrastructure
1. Anthropic + OpenAI API integration with tool calling
2. Observable effect implementation
3. Streaming infrastructure with Conduits
4. AiFileSystem effect with smart filtering

### Phase 2: Core Functionality
1. Essential tool set implementation
2. Rate limiting and cost management
3. CLI chat interface
4. End-to-end testing

### Phase 3: Enhancement & Polish
1. Advanced UI features (diffs, previews)
2. Git integration
3. Package manager awareness
4. Performance optimizations

## Key Design Decisions

### Context Management
- **CLAUDE.md support**: Read at agent startup for project-specific instructions
- **Change-aware file system**: Only re-read modified files
- **Gitignore filtering**: Built into AiFileSystem effect
- **Size limits**: Prevent context explosion from large files

### Security Model
- **Effect constraints**: Tool permissions controlled by effect stack
- **AiFileSystem boundaries**: Separate AI-safe operations from raw file access
- **No arbitrary execution**: Constrained command set instead of shell access

### UI Philosophy
- **CLI-first**: Terminal-native experience
- **Progressive enhancement**: Start simple, add sophistication incrementally
- **Real-time feedback**: Streaming and Observable effects provide immediate updates

## Success Criteria
- Functional coding assistant comparable to Claude Code
- Type-safe, effect-controlled architecture
- Extensible foundation for future agentic capabilities
- Production-ready reliability and security

## Future Evolution Path
Once MVP is stable, layer on:
- Multi-agent coordination
- Specialized agents (debugging, analysis, etc.)
- Self-modification capabilities
- Advanced verification and validation