# Cron Skill

Manage the user's crontab via natural language.

## What it does

Allows an agent to read, add, remove, and edit cron jobs in the user's
crontab.  The agent sees the full crontab text and can modify it using
structured tools rather than having to construct raw `crontab` invocations.

## Tools

### `cron_list`

Returns the current crontab as text, with line numbers prepended.
Comments and blank lines are preserved.

**Parameters** — none

**Returns** The formatted crontab text, or a message if the crontab is empty.

---

### `cron_add`

Appends a new cron entry to the crontab.

**Parameters**

| Name       | Type   | Description                                   |
|------------|--------|-----------------------------------------------|
| `schedule` | string | Cron schedule expression, e.g. `0 * * * *`   |
| `command`  | string | The command to run                            |
| `comment`  | string | Optional human-readable comment (no `#`)      |

**Returns** Confirmation with the line that was added.

---

### `cron_remove`

Removes all cron entries whose command line contains the given pattern.
Requires exactly one match; returns an error if zero or multiple match.

**Parameters**

| Name      | Type   | Description                          |
|-----------|--------|--------------------------------------|
| `pattern` | string | Substring to match against entries   |

**Returns** Confirmation of the removed line, or an error message.

---

### `cron_edit`

Replaces the schedule of an existing cron entry matched by pattern.

**Parameters**

| Name          | Type   | Description                                 |
|---------------|--------|---------------------------------------------|
| `pattern`     | string | Substring to identify the entry to edit     |
| `new_schedule`| string | New cron schedule expression                |

**Returns** The old and new entry for confirmation.

## How the tools work together

The typical interaction pattern:
1. Agent calls `cron_list` to see what currently exists.
2. Agent calls `cron_add`, `cron_remove`, or `cron_edit` based on the request.
3. Agent calls `cron_list` again to confirm the final state.

## Limitations

- Only manages the *current user's* crontab (`crontab -l` / `crontab -`).
- System crontabs (`/etc/cron.d/`, `/etc/crontab`) are not supported.
- Multi-match `cron_remove` and `cron_edit` deliberately fail — ask the user
  to be more specific rather than making ambiguous changes.
- The `@reboot`, `@daily` etc. shorthand schedules are accepted but not
  validated — the underlying `crontab` command will reject invalid input.

## Effects

The skill uses the `Cron` effect, which has two interpreters:

- `cronIO` — the real interpreter; shells out to `crontab` via `Cmd "crontab"`.
- `cronInMemory` — an in-memory interpreter backed by `State CrontabText`;
  used in tests.
