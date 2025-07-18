import numpy as np

# %% compute the matrix after 4 transitions
Q = np.array([[1/3,1/3,1/3, 0  ],
              [0,  0,  1/2, 1/2],
              [0,  1,  0,   0  ],
              [1/2,0,  0,   1/2]], dtype=np.float64)  # fmt: skip

after_5_steps = Q
for i in range(4):
    after_5_steps = after_5_steps.dot(Q)

after_5_steps

# %% simulation


def step(state) -> int:
    to_probabilities = Q[state]
    return np.random.choice([0, 1, 2, 3], p=to_probabilities)


def step_5_times(state) -> int:
    for i in range(4):
        state = step(state)
    return state


stats = np.array([0, 0, 0, 0], dtype=np.int64)
for i in range(10000):
    result_state = step_5_times(0)
    stats[result_state] += 1

print("   Theory ", after_5_steps[0])
print("Simulated ", stats / sum(stats))
