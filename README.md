## Project Overview

This project investigates whether virtual reality (VR) environments based on **hyperbolic** and **fractal** geometry can induce **psychedelic-like effects**—subjectively, neurally, and physiologically—without pharmacological substances.

Participants were immersed in geometric VR scenes while **EEG**, **heart rate**, and **SCR** were recorded. The primary goal was to examine changes in **EEG spectral power** (especially alpha), **Lempel–Ziv complexity (LZc)**, and **autonomic responses**, and to evaluate whether these effects partially resemble signatures reported in psychedelic research.

---

## 🧪 Experimental Setup

### Participants
- **N = 30** healthy right-handed participants
- Age: **18–35 years**

### Stimuli (VR scenes created in Unity + Blender)
A total of **20 VR scenes** were presented across four stimulus categories:

- 🌀 **Hyperbolic Honeycombs** (non-Euclidean; psychedelic-like condition) vs. 🧊 **Control Cubes** (Euclidean control)
- 🔁 **Fractals** (Euclidean fractal geometry; psychedelic-like condition) vs. 🔮 **Kaleidoscopes** (Euclidean control)



### Hardware / Recording
- **Meta Quest 2** VR headset
- **EEG** (scalp recording with EOG channels for eye-movement artifact handling)
- **PPG (heart-rate estimation)** and **SCR** sensors



### Measures
- **EEG spectral power (PSD):** delta, theta, alpha, beta
- **FOOOF spectral parameterization (post hoc exploratory):** periodic and aperiodic components
- **Lempel–Ziv complexity (LZc)**
- **Heart rate** and **skin conductance responses (SCR)**
- **Self-report questionnaires** (modified MEQ-30, SSQ; plus exploratory/open-ended responses)

---




## 📊 Key Results Overview





| Measure | Set 1: Honeycombs vs. Control Cubes | Set 2: Fractals vs. Kaleidoscopes |
|---|---|---|
| **Alpha Power (EEG)** | ✅ **Lower in Honeycombs** | ⚠️ No general Geometry effect; **Geometry × Color** interaction (driven by **Blue Fractal**) |
| **Delta Power (EEG)** | ❌ No significant effects | ✅ **Lower in Fractals** (main effect of Geometry) |
| **LZ Complexity (LZc)** | ❌ No significant effects | ⚠️ **Higher in Fractals** with **Geometry × Color** interaction (strongest in **Blue** and **Red**) |
| **FOOOF (Periodic/Aperiodic)** | ❌ No significant component-level effects after whole-head averaging | ✅ **Blue Fractal:** periodic alpha suppression + aperiodic exponent flattening *(post hoc)* |
| **Heart Rate** | ❌ No significant effects | ⚠️ **Color effect only** (e.g., Purple > Red); no Geometry effect |
| **SCR** | ❌ No significant effects | ❌ No significant effects |
| **Subjective Complexity (post-test)** | ✅ **Honeycombs rated more complex** | ❌ **No Fractal–Kaleidoscope difference** |

---

## 🔎 Hypothesis Evaluation



<details>
<summary><strong>❌ H1 — Subjective effects (mystical experience / spatial orientation)</strong></summary>

<br>

**Prediction:** Participants would report a more pronounced mystical experience and spatial-orientation changes in psychedelic-like VR conditions vs. control conditions.

**Status:** **Not directly testable as originally stated** 

### Why H1 was not directly testable
- The **mystical experience** part of H1 could not be formally tested because the **MEQ-30 Mystical subscale** was excluded from the final questionnaire design.
- Therefore, a formal evaluation of “mystical experience”  was not possible.

### What the available data showed
- Self-report data indicated **low-intensity subjective effects overall** (e.g., slight *Transcendence of Time and Space* on average).
- Ratings of **body-position change** did **not** significantly change across repeated presentations in any category (Honeycombs, Control Cubes, Fractals, Kaleidoscopes).



</details>

<details>
<summary><strong>⚠️ H2 — Neurophysiological effects (alpha ↓, LZc ↑)</strong></summary>

<br>

**Prediction:** Psychedelic-like VR scenes would reduce alpha power and increase neural complexity (LZc), similar to pharmacological psychedelic patterns.

**Status:** **Partially supported**

### 🧠 Alpha power
- ✅ **Set 1 (Honeycombs vs. Control Cubes):** alpha power was **lower** for Honeycombs.
- ⚠️ **Set 2 (Fractals vs. Kaleidoscopes):** alpha differences were **not** a general Fractal main effect; instead, they emerged as a **Geometry × Color interaction**, primarily driven by the **Blue Fractal** condition.

### 🔀 Neural complexity (LZc)
- ⚠️ **Not supported as a general effect across both sets.**
- ❌ **Set 1:** no significant LZc effects.
- ✅ **Set 2:** Fractals showed **higher LZc** than Kaleidoscopes, with a **Geometry × Color interaction** (most pronounced in **Blue** and **Red**).


### ➕ Additional EEG finding (not explicitly predicted in H2)
- ✅ **Set 2:** **delta power was lower in Fractals** than in Kaleidoscopes (main effect of Geometry).


</details>

<details>
<summary><strong>⚠️ H3 — Autonomic arousal (heart rate / SCR)</strong></summary>

<br>

**Prediction:** Psychedelic-like VR (hyperbolic + fractal) would increase physiological arousal (heart rate, SCR).

**Status:** **Not supported in the predicted geometry-specific form**

### ❤️ Heart rate
- ❌ No significant **Geometry** effect in either set.
- ⚠️ In **Set 2**, heart rate varied by **Color** (e.g., Purple > Red), indicating autonomic sensitivity to stimulus features **other than geometry alone**.

### 💧 SCR
- ❌ No significant effects of **Geometry**, **Color**, or **Geometry × Color** in either set.



</details>

---

## 🔬 Exploratory Follow-up Analyses (Post hoc)

<details>
<summary><strong>📈 Spectral parameterization (FOOOF: periodic + aperiodic components)</strong></summary>

<br>

These analyses were **post hoc / exploratory** and were conducted to better characterize the observed EEG effects.

### Set 1 (Honeycombs vs. Control Cubes)
- ❌ No significant component-level effects after whole-head averaging, despite local PSD alpha effects.

### Set 2 (Fractals vs. Kaleidoscopes)
- ✅ The **Blue Fractal** condition showed a distinct component-level pattern:
  - **Periodic alpha suppression**
  - **Flattening of the aperiodic exponent**

> 💡 **Interpretive value:** The Set 2 effect cannot be reduced to a simple alpha-power decrease; it is better described as a **composite spectral pattern** involving concurrent periodic and aperiodic changes.

</details>

---

## 🧭 Take-Home Message

Specific VR geometric configurations—especially the **Blue Fractal** condition—elicited EEG patterns that **partially resemble** signatures often discussed in psychedelic research (e.g., alpha suppression, increased LZc, aperiodic flattening).

However, these effects were **condition-specific**, depended on **Geometry × Color interactions**, and should be interpreted cautiously given methodological limitations (including the fixed first presentation of the Blue Fractal).

Therefore, whether these VR-induced states can be considered genuinely **psychedelic-like** remains **inconclusive**.
## ⚠️ Key Limitations (Brief)

- **Questionnaire limitations (subjective outcomes):** the Russian versions of the MEQ-30 and selected AWE-S items were not formally psychometrically validated, which limits confidence in the subjective self-report findings.

- **Order confound (Blue Fractal):** the **Blue Fractal** was always presented first, so its distinct neural effects may partly reflect **novelty effect** rather than stimulus properties alone.
- **Geometry × color interpretability:** color significantly modulated several EEG effects (especially in Set 2), and the palette structure differed across sets, which limits strong claims about **geometry-only** mechanisms.
- **Sample size / power:** no a priori simulation-based power analysis was conducted for the final linear (mixed) models; effect sizes and confidence intervals were reported to aid interpretation.




## 📌 License

This project is released under the MIT License.
