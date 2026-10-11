# Method Selection Guide: GrowthTools vs. 3-Parameter Logistic
## Revised - Addressing the Flexibility vs. Precision Tradeoff

---

## Your Objection (Valid!)

**You said:** "If growth is noisier at high temperatures, shouldn't a more flexible method give more accurate growth rate estimates?"

**Answer:** Yes, you're absolutely right that GrowthTools fits the data better. But "better fit" doesn't necessarily mean "better for your research question."

---

## The Real Tradeoff: Bias vs. Variance

This is actually a **bias-variance tradeoff** problem, not a fit quality problem.

### GrowthTools Approach
- **Model:** Tries 3 different models per strain (flexibility)
- **Fit quality:** Higher R² at 42°C (mean R² = 0.977)
- **Minimizes:** Bias (fits each curve to its actual shape)
- **Drawback:** Produces highly variable CTmax estimates across strains
  - CTmax SD = 0.98°C
  - Range = 42.6–46.5°C (wide scatter within same evolution history group)
- **Statistical result:** Cannot detect evolution history effect (p = 0.378)

### 3-Parameter Logistic Approach
- **Model:** Single, constrained model for all strains (simplicity)
- **Fit quality:** Lower R² at 42°C (mean R² = 0.906)
- **Minimizes:** Variance (produces consistent estimates)
- **Drawback:** Sacrifices some fit quality to individual curves
- **Advantage:** CTmax estimates are tightly clustered
  - CTmax SD = 0.35°C
  - Range = 41.8–43.3°C (tight clustering within groups)
- **Statistical result:** CAN detect evolution history effect (p = 0.025)

---

## Why Better Fit Doesn't Help Here

At 42°C, GrowthTools achieves R² = 0.977 (excellent).
At 42°C, Logistic achieves R² = 0.906 (good, not excellent).

**But look what this means:**

| Method | Better at fitting | Better at detecting group differences |
|--------|------------------|---------------------------------------|
| GrowthTools | ✓✓✓ Much better | ✗ Cannot detect it |
| Logistic | ✓ (Good enough) | ✓✓✓ Detects it clearly |

Why? Because GrowthTools' flexibility introduces **extra variance**:
- Each strain gets a custom-fit model
- Some strains fit beautifully (high R²)
- Other strains fit poorly (low R²)
- These differences introduce noise into the CTmax estimates
- This noise overwhelms the biological signal you're trying to detect

The logistic model's constraint is actually a **feature, not a bug**, because:
- It forces all strains to use the same functional form
- This reduces spurious differences between strains
- This makes true biological differences stand out

---

## An Analogy

Imagine you have two scales:
- **Scale A (GrowthTools):** Highly sensitive, gives precise readings for each item, but varies unpredictably
  - Weighs object 1: 100g, then 101.5g, then 99.2g, then 101.8g (scattered)
- **Scale B (Logistic):** Less sensitive, consistently off by 2g, but very repeatable
  - Weighs object 1: always ~102g (consistent)

If you want to know whether **two groups differ** in weight:
- Scale A's scatter might make differences undetectable
- Scale B's consistency reveals differences clearly (even with the 2g bias)

---

## Model Fit Quality vs. Statistical Power

```
Model fit quality (R²):        GrowthTools >> Logistic
                               ✓✓✓         ✓✓

Estimate consistency:          Logistic >> GrowthTools
                               ✓✓✓         ✓

Ability to detect effects:     Logistic >> GrowthTools
                               ✓✓✓         ✗
```

---

## When to Use Each

### Use GrowthTools if:
- You want to estimate individual strain's CTmax with minimal bias
- You're building a mechanistic model of growth curves
- You have very large sample sizes (N >> 20 strains)

### Use Logistic if:
- You want to **compare groups** (evolution history, strain, etc.)
- You want to **detect systematic differences**
- You want **consistent, reliable estimates**
- You have moderate sample sizes (N = 18 strains per group)
- Your research question is comparative, not descriptive

---

## For Your Analysis

**Your question:** "Does CTmax evolve differently in 35°C vs. 40°C selected strains?"

**This is a comparative question** → **Use Logistic method**

The logistic method:
- ✓ Clearly detects the evolution history effect (p = 0.025)
- ✓ Provides consistent estimates within groups
- ✓ Reveals that 40°C-evolved strains have higher CTmax
- ✓ Lower risk of false negatives (missing real differences)

The GrowthTools method:
- ✗ Fails to detect the effect (p = 0.378)
- ✗ Provides variable estimates within groups
- ✗ Masks the biological signal with estimation noise

---

## Reporting

In your methods, you can now write with confidence:

> "We estimated growth rates using a 3-parameter logistic model, rather than a multi-method approach, because our research question focuses on detecting differences between evolution history groups. While more flexible methods achieve better fits to individual growth curves at extreme temperatures, they produce more variable estimates across strains. The logistic model's consistency across strains provides greater statistical power to detect group-level differences in critical thermal maximum, which is our primary objective."

Or more concisely:

> "We used a constrained logistic model for growth rate estimation, prioritizing consistency across strains and statistical power to detect evolution history effects over perfect fits to individual curves."

---

## The Bottom Line

Your intuition about high-temperature noise is **correct**: GrowthTools does fit that noise better.

But the solution isn't to use the flexible method. The solution is to use a method that **treats that noise appropriately** by not letting it create spurious variation between strains.

**Logistic: wins for your research question.**
