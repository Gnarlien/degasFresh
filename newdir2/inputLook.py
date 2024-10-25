import numpy as np
import matplotlib.pyplot as plt

def load_file(filename):
    """Load a file with space-separated values."""
    try:
        data = np.loadtxt(filename)
        return data
    except Exception as e:
        print(f"Error loading {filename}: {e}")
        return None

# Load the data from the text files
rho_vals = load_file('rhoVals.txt')
ion_profile = load_file('ionProfile.txt')

# Ensure both files were loaded successfully and their lengths match
if rho_vals is not None and ion_profile is not None:
    if len(rho_vals) == len(ion_profile):
        # Plot ion profile vs rho values
        plt.figure(figsize=(8, 6))
        plt.plot(rho_vals, ion_profile, marker='o', linestyle='-', color='b', label='Ion Profile')

        # Labeling the plot
        plt.title('Ion Profile vs Rho Values')
        plt.xlabel('Rho Values')
        plt.ylabel('Ion Profile')
        plt.grid(True)
        plt.legend()

        # Show the plot
        plt.show()
    else:
        print(f"Mismatch in data lengths: rhoVals({len(rho_vals)}) and ionProfile({len(ion_profile)})")
else:
    print("Failed to load one or both of the files.")
