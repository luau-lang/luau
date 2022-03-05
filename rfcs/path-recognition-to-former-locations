# Path recognition to former locations

## Summary

Make the type checking engine recognize the former locations of Instances being cloned into the player's folder.

## Motivation

The type checking engine should take a glance at one of the "Starter" folders where the object actually belongs to prevent tracking locations. If done so, developers may benefit from the result of what the engine retrieved instead of inspecting the Explorer menu.

## Design

First of all, we need to assign the player. It can either be the LocalPlayer property or a direct reference.

```
local player : Player = game:GetService("Players")[player]
```

Then, assign the variables to either the character of the player, PlayerScripts or PlayerGui folders.

```
local PlayerScripts : PlayerScripts = player:FindFirstChild("PlayerScripts")
local character : Model = player.Character
local PlayerGui : PlayerGui = player:FindFirstChild("PlayerGui")
```

After implementing those above, type checking label should appear whenever typing as follows:

  • If the character is used, it should refer to the "StarterCharacterScripts" folder under the "StarterPlayer" service. If there is nothing underneath the folder, the engine should refer to the default ones replicated. If there is a substitute Instance, the engine should refer to it. If none of the Instances substitute the default ones, the engine should also refer to it.
  • If PlayerGui and PlayerScripts are used, the engine ought to refer to "StarterGui" and "StarterPlayerScripts", respectively.

- Is behavior easy to understand and non-surprising?
  • Sure, it is pretty easy to comprehend.

- Can it be implemented performantly today?
  • Unsure since I don't know how the engine was set up.

- Can it be sandboxed assuming malicious usage?
  • It might be confusing that there is no hierarchical presence beneath them.

- Is it compatible with type checking and other forms of static analysis?
  • Yes, it is fully compatible with the engine and applicable.

## Drawbacks

TBD

## Alternatives

We would write down the former locations first. Then replace with the actual ones where they are supposed to be replicated. But it takes too much time and leads to trying to recall where the change would be.
