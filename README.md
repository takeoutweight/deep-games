# Deep Games

Note: Pull requests for this project are ignored at this time.

Deep Games utilizes UCT and deep neural nets to learn effective strategies for general board-game style rule sets. Lenses and extensible records are used to represent game state.

This project aims to explore the application of various techniques to general game learning:

1. Learning "graceful degradations" of rule sets to aid early learning progress and as a form of regularization.
1. Synthesizing hypothetical positions in game state space to focus on high valence but low frequency, as well as to explore commutativity, translation invariance, etc.
1. Generating puzzles from game states that illustrate specific strategies, via an adversarial approach following the valence metric
