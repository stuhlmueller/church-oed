# Optimal Experimental Design in Church

Main files:

- `screencast/*.mov`: Demonstration of the system (modeling/design expt/run/evaluate loop)
- `exemplar-prototype.experiment`:
  - Generator for stimuli (features and responses)
  - Prototype model
  - Exemplar model (GCM)
- `oed.church`: Main Metropolis-Hastings loop for sampling informative stimuli
- `test-models.church`: Verify that the (random) response distribution of the prototype model is closer to another run of the prototype model than to a run of the exemplar model.
- `test-prt-gcm.church`: Run both prototype and exemplar model on a stimulus that discriminates between the two models. Show the predictions of the two models.
- `oed-utils.church`, `lib/oed.church`: Various helper functions for Church
- `oed-utils.ss`: Sorting, various divergence measures
