# Runix Monorepo

This repository serves as the main development container for the Runix ecosystem, including the core library, supporting libraries, and applications built on top of Runix.

> **⚠️ Early Development Notice**: Runix is in active development. APIs and documentation may change as the project evolves.

## Repository Structure

This monorepo contains:

- **`runix/`** - The core Runix library
  - A secure, modular framework for running and combining type-safe tasks written in Haskell
  - Built on Polysemy effects for composable, controlled access to system resources (FileSystem, LLM, HTTP, etc.)
  - Safe Haskell enforcement for security
  - Separate concerns: effects (what operations are available), runners (how tasks execute), and frontends (how users interact)
  - See [runix/README.md](runix/README.md) for comprehensive documentation

- **`libs/universal-llm/`** - Universal LLM library
  - Haskell library for type-safe interactions with various Large Language Models
  - Extracted as separate library because LLM patterns are broadly applicable
  - Provides the LLM effect that Runix effects can use
  - Supports multiple providers (Anthropic, LlamaCpp, etc.)

- **`apps/runix-code/`** - Real-world test application for Runix
  - AI-powered coding assistant and agent loop built using Runix
  - Demonstrates Runix's capability to build a fully-featured application
  - Features: Brick TUI, tool calling, session management, multi-model support
  - This is more than a demo—it's a real application proving Runix's functionality
  - Also includes simple CLI runner for quick testing

- **`apps/`** - Other example applications
  - `chatbot/` - Chatbot application
  - `helloworld/` - Minimal example application

- **`tasks/`** - Example tasks demonstrating Runix capabilities
  - Shows how to create, compose, and test tasks
  - Real examples of effect usage

- **`templates/`** - Project templates for creating new Runix tasks and applications

## Getting Started

### Prerequisites

This project uses Nix for reproducible development environments and builds. You'll need:
- Nix with flakes enabled
- Git (for submodules)

### Setting Up the Development Environment

The `flake.nix` file provides a complete development shell with all necessary tools:

```bash
# Enter the development shell
nix develop

# This provides:
# - GHC (Glasgow Haskell Compiler)
# - Cabal 3.14.2.0 (for HLS compatibility)
# - Haskell Language Server
# - Required Haskell libraries (polysemy, aeson, http-conduit, etc.)
# - Hoogle documentation
```

### Building

The cabal project includes all packages defined in `cabal.project`:

```bash
# Build all packages
cabal build all

# Build specific package
cabal build runix
cabal build runix-code

# Run tests
cabal test all
```

### Running Applications

#### runix-code

Runix-code is an AI-powered coding assistant. To run it:

```bash
# Set required environment variable
export ANTHROPIC_OAUTH_TOKEN="your-anthropic-oauth-token"

# Optional: Select model (defaults to claude-sonnet-45)
export RUNIX_MODEL="claude-sonnet-45"  # or "glm-45-air", "qwen3-coder"

# For LlamaCpp models (qwen3-coder), set the endpoint
export LLAMACPP_ENDPOINT="http://localhost:8080/v1"  # default if not set

# Run the TUI
cabal run runix-code:runix-code-tui

# Or build and run via nix
nix build
./result/bin/runix-code-tui
```

**Environment Variables:**
- `ANTHROPIC_OAUTH_TOKEN` - **Required** for Claude models (claude-sonnet-45)
- `RUNIX_MODEL` - Model selection: `"claude-sonnet-45"` (default), `"glm-45-air"`, or `"qwen3-coder"`
- `LLAMACPP_ENDPOINT` - LlamaCpp server endpoint for local models (default: `http://localhost:8080/v1`)

## Project Status

This is the primary development repository containing the Runix framework and real-world applications built with it.

### Current State

- **Runix library**: Active development, core functionality stable
- **Universal-LLM library**: Active development, core functionality stable
- **runix-code application**: Active development, beyond-demo stage with real features
- **Other applications**: Early stage, more experimental
- **Documentation**: In progress, may be incomplete or inconsistent

### Development Philosophy

The project emphasizes:
- **Type safety**: Extensive use of Haskell's type system
- **Security**: Safe Haskell and controlled effects
- **Modularity**: Composable tasks and effects
- **Reproducibility**: Nix-based builds and dependency management

## Documentation

- **Runix Core**: See [runix/README.md](runix/README.md) for detailed documentation on the framework, task development, and effect system
- **Task Creation**: See [runix/howto-create-tasks-with-ai.md](runix/howto-create-tasks-with-ai.md)
- **Agentic Design**: See [agentic-runix.md](agentic-runix.md)

## Contributing

As this project is in early development, contribution guidelines are still being formalized. The codebase prioritizes:
- Clear, elegant solutions over quick workarounds
- Well-understood tradeoffs when compromises are necessary
- Type-safe, composable designs

## License

See individual submodules for their respective licenses.

## Links

- Issues and discussions: (To be added when repository is published)
- Documentation: (To be added)

---

For questions or issues during this early development phase, please refer to the documentation in the respective submodule directories.
