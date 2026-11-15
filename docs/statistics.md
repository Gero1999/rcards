# Statistical estimation contract

Estimates use public observations, never the actual hidden hands or future deck
order. Card-only strength, expected action profit, and strategy-dependent game
outcomes are different questions. Reports state method, assumptions, sample
counts, and 95% uncertainty intervals. Sampling intervals quantify Monte Carlo
noise only, not wrong assumptions about players or missing information.

`draw_probability(successes, remaining, draws)` computes the exact chance of at
least one success in uniform draws without replacement, without simulation.
It is useful for known outs, not a substitute for showdown equity.

Probability intervals use Wilson intervals, so zero observed successes does not
mean a proven zero probability. Mean/equity intervals use a normal Monte Carlo
approximation; one sample has no estimated standard error. Exact enumeration
has zero sampling error but still depends on its stated model. No API calls,
external lookup tables, or binary datasets are required.
