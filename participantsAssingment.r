# Random assignment of impairments to participants
input_path <- "/Users/carbeluche/Desktop/TFG/participants.csv"
output_path <- "/Users/carbeluche/Desktop/TFG/random_disability_assignment.csv"

# Load participant data
participants <- read.csv(input_path, sep = ";", header = FALSE, stringsAsFactors = FALSE)
head(participants)

# Assign column names missing and define the disabilities
colnames(participants) <- c("Name", "Gender", "Age")
disabilities <- c("motor", "visual", "auditory")

# Set seed for reproducibility
set.seed(42)

# Number of participants
n <- nrow(participants)

# Add empty columns for simulation order
participants$First <- character(n)
participants$Second <- character(n)
participants$Third <- character(n)

# Assign a random order per participant
for (i in 1:n) {
  order <- sample(disabilities)
  participants[i, c("First", "Second", "Third")] <- order
}

# Save the result
write.csv(participants, output_path, row.names = FALSE)
