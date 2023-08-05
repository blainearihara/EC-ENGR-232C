import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
import os

#%%
def generate_heat_map(R_function, title):
    R_function = np.array(R_function)
    xdim,ydim = R_function.shape
    x = np.arange(0,xdim+1,1)
    y = np.arange(0,ydim+1,1)
    
    X, Y = np.meshgrid(x, y)
    fig, ax = plt.subplots(figsize=(8,6))
    ax.invert_yaxis()
    ax.xaxis.tick_top()
    
    # set the x ticks
    x_tick_positions = list(range(0, xdim+1))  # [1, 2, 3, ..., 10]
    x_tick_labels = [str(tick) for tick in x_tick_positions]
    ax.set_xticks(x_tick_positions)
    ax.set_xticklabels(x_tick_labels)
    
    # set the y ticks
    y_tick_positions = list(range(0, ydim+1))  # [1, 2, 3, ..., 10]
    y_tick_labels = [str(tick) for tick in y_tick_positions]
    ax.set_yticks(y_tick_positions)
    ax.set_yticklabels(y_tick_labels)
    
    fig.subplots_adjust(right=0.8, top=0.85)
    map = ax.pcolor(X, Y, R_function, cmap="cividis", vmax=R_function.max(), vmin=R_function.min())
    cbar_ax = fig.add_axes([0.82, 0.12, 0.03, 0.68])
    fig.colorbar(map, cax=cbar_ax)
    ax.set_title(title, y=1.08)
    ax.grid(True, color='black', linewidth=0.2)
    
    
    ind_it = title.find("iteration")
    ind_re = title.find("reward")
    filename = f"Q9_HeatMap {title[ind_re:ind_re+17]} {title[ind_it:ind_it+13]}"
    parent_directory = os.path.abspath(os.path.join(os.getcwd(), ".."))
    full_file_path = os.path.join(parent_directory, filename)
    plt.savefig(full_file_path, bbox_inches='tight')
    
    plt.show()

reward_function_1 = np.zeros([10, 10])
reward_function_1[4:6,1:3] = -1
reward_function_1[8:10,2:4] = -1
reward_function_1[2:4,5:7] = -1
reward_function_1[9,9] = 1.
title1 = "Heat map of reward function 1"
generate_heat_map(reward_function_1, title1)

reward_function_2 = np.zeros([10, 10])
reward_function_2[1:7,4] = -100
reward_function_2[1,5] = -100
reward_function_2[[1,2,3,7,8],6] = -100
reward_function_2[[3,7],7] = -100
reward_function_2[3:8,8] = -100
reward_function_2[9,9] = 10
title2 = "Heat map of reward function 2"
generate_heat_map(reward_function_2, title2)

#%%

def plot_optimal_value_grid(V_function, title):
    V_function = np.array(V_function)
    ydim,xdim = V_function.shape
    
    fig, ax = plt.subplots(figsize=(8,6))
    fig.subplots_adjust(right=0.8, top=0.85)
    ax.invert_yaxis()
    ax.xaxis.tick_top()
    ax.set_title(title, y=1.08)
    ax.pcolor(V_function, cmap=ListedColormap(['white']), edgecolors='black')
    
    # set the x ticks
    x_tick_positions = list(range(0, xdim+1))  # [1, 2, 3, ..., 10]
    x_tick_labels = [str(tick) for tick in x_tick_positions]
    ax.set_xticks(x_tick_positions)
    ax.set_xticklabels(x_tick_labels)
    
    # set the y ticks
    y_tick_positions = list(range(0, ydim+1))  # [1, 2, 3, ..., 10]
    y_tick_labels = [str(tick) for tick in y_tick_positions]
    ax.set_yticks(y_tick_positions)
    ax.set_yticklabels(y_tick_labels)
    
    for y in range(ydim):
        for x in range(xdim):
            ax.text(x + 0.5, y + 0.5, '%0.2f' % V_function[y,x], horizontalalignment='center', verticalalignment='center',fontsize=7.5)
    
    
    ind_it = title.find("iteration")
    ind_re = title.find("reward")
    filename = f"Q9_ValueMap {title[ind_re:ind_re+17]} {title[ind_it:ind_it+13]}"
    parent_directory = os.path.abspath(os.path.join(os.getcwd(), ".."))
    full_file_path = os.path.join(parent_directory, filename)
    plt.savefig(full_file_path, bbox_inches='tight')
    
    plt.show()


def plot_optimal_policy_grid(Pi_function, title):
    Pi_function = np.array(Pi_function)
    ydim,xdim = Pi_function.shape
    arrow_map = np.chararray([ydim,xdim], unicode=True)
    arrows = ['\u2191', '\u2193', '\u2192', '\u2190']  # (up), (down), (right), (left)
    
    
    fig, ax = plt.subplots(figsize=(8,6))
    fig.subplots_adjust(right=0.8, top=0.85)
    ax.invert_yaxis()
    ax.xaxis.tick_top()
    ax.set_title(title, y=1.08)
    
    # set the x ticks
    x_tick_positions = list(range(0, xdim+1))  # [1, 2, 3, ..., 10]
    x_tick_labels = [str(tick) for tick in x_tick_positions]
    ax.set_xticks(x_tick_positions)
    ax.set_xticklabels(x_tick_labels)
    
    # set the y ticks
    y_tick_positions = list(range(0, ydim+1))  # [1, 2, 3, ..., 10]
    y_tick_labels = [str(tick) for tick in y_tick_positions]
    ax.set_yticks(y_tick_positions)
    ax.set_yticklabels(y_tick_labels)
    
    
    for y in range(ydim):
        for x in range(xdim):
            if Pi_function[y,x] == 0:
                arrow_map[y,x] = arrows[0]
            elif Pi_function[y,x] == 1:
                arrow_map[y,x] = arrows[1]
            elif Pi_function[y,x] == 2:
                arrow_map[y,x] = arrows[2]
            elif Pi_function[y,x] == 3:
                arrow_map[y,x] = arrows[3]
            ax.text(x + 0.5, y + 0.5, arrow_map[y,x], horizontalalignment='center', verticalalignment='center',fontsize=22)

    ind_re = title.find("reward")
    filename = f"Q9_ArrowMap {title[ind_re:ind_re+17]}"
    parent_directory = os.path.abspath(os.path.join(os.getcwd(), ".."))
    full_file_path = os.path.join(parent_directory, filename)
    plt.savefig(full_file_path, bbox_inches='tight')
    
    plt.show()
    

#%%
# Create the environment of the agent using the information provided in 
# section 2. To be specific, create the MDP by setting up the state-space,
# action set, transition probabilities, discount factor, and reward function.

# Create state space, action set, transition probabilities, discount factor, and reward function       
class Grid:
    
    def __init__(self, grid_size, wind, discount, epsilon):

        
        self.actions = np.array([0,1,2,3]) # (up), (down), (right), (left)    
        self.w = wind #probability / wind
        self.gamma = discount
        self.epsilon = epsilon
        self.grid_size = grid_size
        # self.state_space = np.tile(np.arange(0, self.grid_size**2, self.grid_size), (self.grid_size, 1)) + np.arange(0, self.grid_size, 1).reshape((self.grid_size, 1))
        self.state_space = np.arange(self.grid_size**2).reshape(self.grid_size,self.grid_size).transpose()
        
    def get_coordinates(self, state):
        # return the x, y coordinates of the location of the entity on the state_space
        y,x = np.where(self.state_space == state)
        return(x,y)
    

    def get_next_state(self, state, move):
        # get coordinates of the current state
        x,y = self.get_coordinates(state)
        
        # update the coordinates according the move
        if move == 0:
            y -= 1
        elif move == 1:
            y += 1
        elif move == 2:
            x += 1
        elif move == 3:
            x -= 1
        elif move == 4:
            pass
        else:
            print("Invalid move " + str(move) + " at state " + str(state))            
        return self.state_space[y,x]
    
    def get_reward(self, state, reward_function):
        x,y = self.get_coordinates(state)
        return(reward_function[y,x])
    
    def get_valid_moves(self, state):
        # remove the invalid moves that would move entity off the state_space
        valid_moves = self.actions
        x,y = self.get_coordinates(state)
        if y == 0: # on the top edge
            valid_moves = np.delete(valid_moves, np.where(valid_moves == 0))
        elif y == 9: # on the bottom edge
            valid_moves = np.delete(valid_moves, np.where(valid_moves == 1))
        if x == 9: # on the right edge
            valid_moves = np.delete(valid_moves, np.where(valid_moves == 2))
        elif x == 0: # on the left edge
            valid_moves = np.delete(valid_moves, np.where(valid_moves == 3))
        return(valid_moves)
    
    def get_transition_probabilities(self, state, action):
        # set up the transition_probabilities to account for non-boundary, boundary, and corner cases
        valid_moves = self.get_valid_moves(state)
        # Transition Probabilities contains 5 elements. 4 for up, down, right, left. and the 5th for stay
        transition_probabilities = np.zeros(5) # P(s',s,a)
        
        # non-boundary case
        if len(valid_moves) == 4:
            transition_probabilities[0:4] = self.w/4
            transition_probabilities[action] += 1 - self.w
        # boundary case    
        elif len(valid_moves) == 3:
            invalid_move_ind = np.where(~np.isin(self.actions, valid_moves))
            valid_move_ind = valid_moves
            transition_probabilities[valid_move_ind] = self.w/4
            # if action is to be blown off the grid
            if invalid_move_ind == action:
                transition_probabilities[4] += 1 - self.w
            # if action stays in the grid
            else:
                transition_probabilities[action] += 1 - self.w
        elif len(valid_moves) == 2:
            invalid_move_ind = np.where(~np.isin(self.actions, valid_moves))
            valid_move_ind = valid_moves
            transition_probabilities[valid_move_ind] = self.w/4
            # if action is to be blown off the grid
            if np.any(np.isin(invalid_move_ind, action)):
                transition_probabilities[4] += 1 - self.w + self.w/4
            # if action stays in the grid
            else:
                transition_probabilities[action] += 1 - self.w
                transition_probabilities[4] += self.w/4*2
                
        return(transition_probabilities)
        
        
    def get_optimal_value_function(self, reward_function):
        # Initialization
        optimal_value_function = np.zeros((self.grid_size, self.grid_size)) # state value function
        delta = 1
        iterations = 0
        # Estimation
        while delta > self.epsilon:
            iterations += 1
            delta = np.array([0])
            state_value_function = np.copy(optimal_value_function)
            
            # for every state s in the State Set
            for s in range(np.size(self.state_space)):
                # find value function of current state, s
                sx, sy = self.get_coordinates(s)
                state_value = state_value_function[sy,sx]
                next_state_value_actions = np.zeros(len(self.actions))        
                
                # for every action in the Action Set
                for a in self.actions:
                    # find the transition probability 
                    state_transition_probabilities = self.get_transition_probabilities(s, a)
                    
                    # for every possible next state s' in the State Set
                    for p in range(len(state_transition_probabilities)):
                        # skip the invalid moves
                        if state_transition_probabilities[p] != 0:
                            # find state after transition
                            next_state = self.get_next_state(s,p)
                            # find reward of transition state
                            next_reward = self.get_reward(next_state, reward_function)
                            # find next state value
                            next_x, next_y = self.get_coordinates(next_state)
                            next_state_value = optimal_value_function[next_y, next_x]
                            # calculate
                            next_state_value_actions[a] += state_transition_probabilities[p] * (next_reward + self.gamma*next_state_value)
            
                max_state_value = np.amax(next_state_value_actions)
                optimal_value_function[sy,sx] = max_state_value
                delta = max([delta, abs(state_value - max_state_value)])
                # if iterations == 1 or iterations % 4 == 0 or delta < self.epsilon:
            generate_heat_map(optimal_value_function, f'Heat map of state values for reward function 1\n at iteration {iterations}')
            plot_optimal_value_grid(optimal_value_function, f'Optimal Value map of state values for reward function 1\n at iteration {iterations}')
        return(iterations, optimal_value_function)
    
    def get_optimal_policy_function(self, optimal_value_function, reward_function):
        # Initialization
        optimal_policy_function = np.zeros((self.grid_size, self.grid_size)) # optimal policy function
        
        # for every state s in the State Set
        for s in range(np.size(self.state_space)):
            # find value function of current state, s
            sx, sy = self.get_coordinates(s)
            next_state_value_actions = np.zeros(len(self.actions))        
            
            # for every action in the Action Set
            for a in self.actions:
                # find the transition probability 
                state_transition_probabilities = self.get_transition_probabilities(s, a)
                
                # for every possible next state s' in the State Set
                for p in range(len(state_transition_probabilities)):
                    # skip the invalid moves
                    if state_transition_probabilities[p] != 0:
                        # find state after transition
                        next_state = self.get_next_state(s,p)
                        # find reward of transition state
                        next_reward = self.get_reward(next_state, reward_function)
                        # find next state value
                        next_x, next_y = self.get_coordinates(next_state)
                        next_state_value = optimal_value_function[next_y, next_x]
                        # calculate
                        next_state_value_actions[a] += state_transition_probabilities[p] * (next_reward + self.gamma*next_state_value)
            
            # find the action that received the highest value, indicating the optimal action to take
            optimal_action = np.argmax(next_state_value_actions)
            optimal_policy_function[sy,sx] = optimal_action
        return(optimal_policy_function.astype(int))

grid_size = 10
wind = 0.01
discount_factor = 0.8
epsilon = 0.01

grid = Grid(grid_size, wind, discount_factor, epsilon)
iterations, optimal_value_function = grid.get_optimal_value_function(reward_function_1)

optimal_policy_function = grid.get_optimal_policy_function(optimal_value_function, reward_function_1)
plot_optimal_policy_grid(optimal_policy_function, 'Optimal Policy map for reward function 1')

# print(pd.DataFrame(optimal_value_function))
# generate_heat_map(optimal_value_function, 'Heat map of state values using reward function 1')




