# coroutine.close

## Summary

Add `coroutine.close` function from Lua 5.4 that takes a suspended coroutine and makes it "dead" (non-runnable).

**Status**: Implemented

## Motivation

When implementing various higher level objects on top of coroutines, such as promises, it can be useful to cancel the coroutine execution externally - when the caller is not
interested in getting the results anymore, execution can be aborted. Since coroutines don't provide a way to do that externally, this requires the framework to implement
cancellation on top of coroutines by keeping extra status/token and checking that token in all places where the coroutine is resumed.

Since coroutine execution can be aborted with an error at any point, coroutines already implement support for "dead" status. If it were possible to externally transition a coroutine
to that status, it would be easier to implement cancellable promises on top of coroutines.

## Design

We implement Lua 5.4 behavior exactly with the exception of to-be-closed variables that we don't support. Quoting Lua 5.4 manual:

> coroutine.close (co)
> Closes coroutine co, that is, puts the coroutine in a dead state. The given coroutine must be dead or suspended. In case of error (either the original error that stopped the coroutine or errors in closing methods), returns false plus the error object; otherwise returns true.

The `co` argument must be a coroutine object (of type `thread`).

After closing the coroutine, it gets transitioned to dead state which means that `coroutine.status` will return `"dead"` and attempts to resume the coroutine will fail. In addition, the coroutine stack (which can be accessed via `debug.traceback` or `debug.info`) will become empty. Calling `coroutine.close` on a closed coroutine will return `true` - after closing, the coroutine transitions into a "dead" state with no error information.

## Drawbacks

None known, as this function doesn't introduce any existing states to coroutines, and is similar to running the coroutine to completion/error.

## Alternatives

Lua's name for this function is likely in part motivated by to-be-closed variables that we don't support. As such, a more appropriate name could be `coroutine.cancel` which also
aligns with use cases better. However, since the semantics is otherwise the same, using the same name as Lua 5.4 reduces library fragmentation.
