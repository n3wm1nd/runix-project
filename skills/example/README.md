# Example Skill

A minimal reference skill for the Runix skill scaffold.

## What it does

Exposes a single `echo` tool that returns whatever text the user passes in.
Useful for verifying that the skill infrastructure (tool dispatch, test harness,
caching layer) is wired up correctly end-to-end.

## Tools

### `echo`

**Parameters**

| Name    | Type   | Description              |
|---------|--------|--------------------------|
| `input` | string | The text to echo back    |

**Returns** The input text, unchanged.

## Limitations

This skill does nothing useful.  It is purely a scaffold reference.

## Usage

Enable by including `exampleSkill` in your agent's tool list:

```haskell
import Skill.Example (exampleSkill)

myTools = skillTools exampleSkill ++ otherTools
```
