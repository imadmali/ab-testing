# Probabilistic A/B Testing with Stan

Statisticians, Data Scientists, etc. should not make business decisions; they should facilitate them. This case study shows how basic A/B testing using Stan and Bayesian methods can achieve this. In practice, we find this approach useful given its ability to quantify domain-specific business knowledge and hypotheses through the use of prior distributions. Instead of using p-values and confidence intervals, we are able to perform inference with probability and credible intervals directly on posterior predictions. In addition to our results being highly interpretable, this approach gives us the ability to quantify business risk.

Case study is available in `doc/bayes_test.html`

Note that these tests go over A/B testing where inference takes place after the experiment. For tests that allow inference to occur during the experiment the reader should consider the multi-arm bandit testing approach.
