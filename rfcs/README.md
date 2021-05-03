Background
===

Whenever Luau language changes its syntax or semantics (including behavior of builtin libraries), we need to consider many implications of the changes.

Whenever new syntax is introduced, we need to ask:

- Is it backwards compatible?
- Is it easy for machines and humans to parse?
- Does it create grammar ambiguities for current and future syntax?
- Is it stylistically coherent with the rest of the language?
- Does it present challenges with editor integration like autocomplete?

For changes in semantics, we should be asking:

- Is it easy to understand and non-surprising?
- Can it be implemented performantly today?
- Can it be sandboxed assuming malicious usage?
- Is it compatible with type checking and other forms of static analysis?

In addition to these questions, we also need to consider that every addition carries a cost, and too many features will result in a language that is harder to learn, harder to implement and ensure consistent implementation quality throughout, slower, etc. In addition, any language is greater than the sum of its parts and features often have non-intuitive interactions with each other.

Since reversing these decisions is incredibly costly and can be impossible due to backwards compatibility implications, all user facing changes to Luau language and core libraries must go through an RFC process.

Process
===

To open an RFC, a Pull Request must be opened which creates a new Markdown file in rfcs/ folder. The RFCs should follow the template `rfcs/TEMPLATE.md`, and should have a file name that is a short human readable description of the feature (using lowercase alphanumeric characters and dashes only).

> Note: we currently don't accept community contributions for RFCs, although this will likely change in the future.

Every open RFC will be open for at least two calendar weeks. This is to make sure that concerns regarding any points raised above are addressed. The discussion points should be reflected on the PR comments; when discussion happens outside of the comment stream, the points salient to the RFC should be summarized as a followup.

When the initial comment period expires, the RFC can be merged if there's consensus that the change is important and that the details of the syntax/semantics presented are workable. The decision to merge the RFC is made by the Luau team.

When revisions on the RFC text that affect syntax/semantics are suggested, they need to be incorporated before a RFC is merged; a merged RFC represents a maximally accurate version of the code change that is going to follow.

In some cases RFCs may contain conditional compatibility clauses. E.g. there are cases where a change is potentially not backwards compatible, but is believed to be substantially beneficial that it can be implemented if, in practice, the backwards compatibility implications are minimal. As a strawman example, if we wanted to introduce a non-context-specific keyword `globally_coherent_buffer`, we would be able to do so if our analysis of Luau code (based on the Roblox platform at the moment) informs us that no script in existence uses this keyword. In cases like this an RFC may need to be revised after the initial implementation attempt based on the data that we gather.

In general, RFCs can also be updated after merging to make the language of the RFC more clear, but should not change their meaning. When a new feature is built on top of an existing feature that has an RFC, a new RFC should be created instead of editing an existing RFC.

When there's no consensus that the feature is broadly beneficial and can be implemented, an RFC will be closed. The decision to close the RFC is made by the Luau team.

Note that in some cases an RFC may be closed because we don't have sufficient data or believe that at this point in time, the stars do not line up sufficiently for this change to be worthwhile, but this doesn't mean that it may never be considered again; an RFC PR may be reopened if new data is available since the original discussion, or if the PR has changed substantially to address the core problems raised in the prior round.

Implementation
===

When an RFC gets merged, the feature *can* be implemented; however, there's no set timeline for that implementation. In some cases implementation may land in a matter of days after an RFC is merged, in some it may take months.

To avoid having permanently stale RFCs, in rare cases Luau team can *remove* a previously merged RFC when the landscape is believed to change enough for a feature like this to warrant further discussion.

To track progress of implemented features, it's advised to follow documentation instead; while RFCs aren't meant to be user-facing documentation, recaps in particular can reference RFCs for additional context behind the changes.
