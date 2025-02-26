import tkinter as tk
from tkinter import ttk
from sklearn.ensemble import RandomForestRegressor
from sklearn.preprocessing import StandardScaler
import numpy as np
import pandas as pd
import warnings
warnings.filterwarnings('ignore')

# Load dataset
data = pd.read_csv('housing.csv')
X = data.drop('MEDV', axis=1)
y = data['MEDV']

def get_partition_means(feature):
    
    sorted_feature = np.sort(feature) 
    third = len(sorted_feature) // 3  # 20
    
    # Get the means of each part
    low_mean = np.mean(sorted_feature[:third])
    medium_mean = np.mean(sorted_feature[third:2*third])
    high_mean = np.mean(sorted_feature[2*third:])
    
    return low_mean, medium_mean, high_mean

# Calculate means for LSTAT and PTRATIO
lstat_low, lstat_medium, lstat_high = get_partition_means(data['LSTAT'])
ptratio_low, ptratio_medium, ptratio_high = get_partition_means(data['PTRATIO'])

# Standardize features
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# Train model
model = RandomForestRegressor(random_state=42, n_estimators=100, max_depth=5)
model.fit(X_scaled, y)

def predict_price():
    try:
        rm = int(entry_rm.get())
        
        # Convert LSTAT and PTRATIO from text to numeric based on the selected level
        lstat_mapping = {"low": lstat_low, "medium": lstat_medium, "high": lstat_high}
        ptratio_mapping = {"low": ptratio_low, "medium": ptratio_medium, "high": ptratio_high}

        lstat = lstat_mapping[combo_lstat.get()]
        ptratio = ptratio_mapping[combo_ptratio.get()]

        input_features = scaler.transform([[rm, lstat, ptratio]])
        prediction = model.predict(input_features)[0]

        result_label.config(text=f"Predicted Price: ${prediction:,.2f}", foreground="#0078D7")
    except ValueError:
        result_label.config(text="Please enter valid numeric values.", foreground="red")

# GUI Setup
root = tk.Tk()
root.title("Housing Price Prediction")
root.geometry("550x400")
root.configure(bg="#ffffff")

# Define the custom style for the button
style = ttk.Style()
style.configure("TButton", foreground="black", padding=10, relief="flat", font=("Helvetica", 12), background="#4CAF50")
style.map("TButton", background=[("active", "#388E3C")])
style.configure("TLabel", font=("Helvetica", 12, "bold"), background="#ffffff", foreground="#333")
style.configure("TEntry", font=("Helvetica", 12), padding=5)
style.configure("TCombobox", font=("Helvetica", 12), padding=5)

# Header Frame
header_frame = tk.Frame(root, bg="#4CAF50", pady=10)
header_frame.pack(fill="x")
header_label = tk.Label(header_frame, text="🏡 Housing Price Prediction 🏡", font=("Helvetica", 16, "bold"), bg="#4CAF50", fg="white")
header_label.pack()

# Input Frame
frame = ttk.Frame(root, padding="20")
frame.pack(padx=30, pady=30, expand=True)

# Room input
label_rm = ttk.Label(frame, text="Number of Rooms (RM):")
label_rm.grid(row=0, column=0, sticky="w", pady=5)
entry_rm = ttk.Entry(frame, width=20)
entry_rm.grid(row=0, column=1, pady=5, padx=15)

# LSTAT input - Combobox for low, medium, high
label_lstat = ttk.Label(frame, text="Population Stats (LSTAT):")
label_lstat.grid(row=1, column=0, sticky="w", pady=5)
combo_lstat = ttk.Combobox(frame, values=["low", "medium", "high"], width=17)
combo_lstat.grid(row=1, column=1, pady=5, padx=15)

# PTRATIO input - Combobox for low, medium, high
label_ptratio = ttk.Label(frame, text="Student-Teacher Ratio (PTRATIO):")
label_ptratio.grid(row=2, column=0, sticky="w", pady=5)
combo_ptratio = ttk.Combobox(frame, values=["low", "medium", "high"], width=17)
combo_ptratio.grid(row=2, column=1, pady=5, padx=15)

# Predict Button
predict_button = ttk.Button(frame, text="Predict Price", command=predict_price, style="Custom.TButton")
predict_button.grid(row=3, column=0, columnspan=2, pady=15)

# Result Label
result_label = tk.Label(root, text="", font=("Helvetica", 14, "bold"), bg="#ffffff", fg="#0078D7")
result_label.pack(pady=20)

# Run the GUI loop
root.mainloop()
