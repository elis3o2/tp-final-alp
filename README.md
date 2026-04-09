# Prob — Probability Language Interpreter

A domain-specific language for probability distributions and Markov chains.

## Requirements

- [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/)
- [gnuplot](http://www.gnuplot.info/) — required for `plot` on distributions
- [graphviz](https://graphviz.org/) — required for `plot` on Markov chains

Install system dependencies:
```bash
# Ubuntu / Debian
sudo apt install gnuplot graphviz

```

---

## Running the Interpreter

**Execute a script:**
```bash
cabal run tp-final-alp -- [OPTIONS] file.prob
```

**Interactive mode (REPL):**
```bash
cabal run tp-final-alp -- [OPTIONS]
```

### Options

| Flag | Description | Default |
|------|-------------|---------|
| `-v`, `--verbose` | Print each command and its result | off |
| `-d N`, `--decimals N` | Number of decimal places to show | 6 |

---

## REPL Commands

| Command | Description |
|---------|-------------|
| `:q`, `:quit` | Exit the REPL |
| `:help` | Show available commands |
| `:env` | Show all declared variables and their types |
| `:load <file>` | Load and execute a script file |

---

## Language Reference

### Declarations

Each declaration must occupy exactly one line.

```
x = <numExpr>          -- numeric variable
x = <vecExpr>          -- vector variable
x = <pathExpr>         -- path variable
X ~ <randExpr>         -- random variable
X := <randExpr>        -- random variable (alternative syntax)
X = <markovExpr>       -- Markov chain
name -> <nodeExpr>     -- Markov chain node
```

> Identifiers starting with **uppercase** are used for random variables
> and Markov chains. Identifiers starting with **lowercase** are used for
> numeric, vector and path variables.

---

### Commands

```
print(e)              -- evaluate and print expression e
table(X)              -- show probability table of discrete X
table(X, lo, hi)      -- show probability table in range [lo, hi]
plot(X)               -- plot distribution of X (generates .png)
plot(M)               -- plot Markov chain M as a directed graph (generates .png)
```

---
### Literal Expressions
```
n                     -- numeric literal (integer or decimal)
(e1, e2, ..., en)     -- vector literal
[s1, s2, ..., sk]     -- path literal
```

---

### Numeric Operations
```
-e                    -- unary minus
e1 + e2
e1 - e2
e1 * e2
e1 / e2
```

---

### Vector Operations

```
v[i]                  -- vector access at index i
```

---

### Statistical Functions

```
E(X)                  -- expected value
V(X)                  -- variance
SD(X)                 -- standard deviation
mode(X)               -- mode (returns a vector)
pdf(X, x)             -- probability density at x  (continuous only)
maxP(X)               -- maximum probability value (discrete only)
maxPDF(X)             -- maximum density value     (continuous only)
```

---

### Discrete Distributions

```
Bin(n, p)             -- Binomial(n, p)
Binomial(n, p)

Poi(l)                -- Poisson(lambda)
Poisson(l)

Geo(p)                -- Geometric(p)
Geometric(p)

BN(r, p)              -- Pascal / Negative Binomial(r, p)
Pascal(r, p)

HG(m, r, n)           -- Hypergeometric(m, r, n)
Hipergeometric(m, r, n)

[xs  ps]              -- Custom distribution
                      --   xs: vector of integer values
                      --   ps: vector of probabilities (must sum to 1)
```

---

### Continuous Distributions

```
N(mu, sigma)          -- Normal(mu, sigma)
Normal(mu, sigma)

Exp(lambda)           -- Exponential(lambda)
Exponential(lambda)

Unif(a, b)            -- Uniform(a, b)
Uniform(a, b)
```

---

### Probability Queries

```
P(X op k)             -- P(X op k)
P(k op X)             -- equivalent form
P(k1 op1 X op2 k2)   -- P(k1 op1 X op2 k2)
```

Available operators: `<`, `<=`, `>`, `>=`, `=`, `/=`

---

### Markov Chains

**Definition:**
```
-- Declare nodes first
state1 -> [(state2, p1), (state3, p2)]
state2 -> [(state1, p3), (state3, p4)]
state3 -> [(state1, p5), (state2, p6), (state3, p7)]

-- Then build the chain
M = mk(state1, state2, state3)
```

**Queries:**
```
F n(M, i, j)          -- probability of going from i to j in n steps
F(M, path)            -- probability of a given path
F(M, i, j)            -- hitting probability: reaching j from i
stationary(M)         -- stationary distribution (returns a vector)
ex(M, n)              -- Markov chain after n transition steps
```

**Simulation:**
```
simulate(M, n, start = s)     -- simulate n steps starting from state s
simulate(M, n, prob = v)      -- simulate n steps with initial distribution v
```

---


### Comments

```
// single line comment

/* multi
   line
   comment */
```

---

## Examples

The `examples/` folder contains sample scripts covering all major features.

```bash
cabal run tp-final-alp -- examples/probabilities.prob
cabal run tp-final-alp -- examples/mk1.prob
cabal run tp-final-alp -- --verbose examples/plots.prob
```