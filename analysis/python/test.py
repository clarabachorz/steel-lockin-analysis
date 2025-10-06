import csv
import os

# go up two directories
grandparent_dir = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
# Path to the input_data folder one level above
input_data_dir = os.path.join(grandparent_dir, "inputdata", "gem")
# Write hello world to CSV
output_file = os.path.join(input_data_dir, "test_output.csv")

with open(output_file, 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(["message", "value"])
    writer.writerow(["Hello World", 42])
    writer.writerow(["Test successful", 100])

print(f"CSV saved to: {output_file}")