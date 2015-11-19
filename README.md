# Churn-Analysis---Telecom
The goal is to predict the propensity to churn of a customer from the service provider through the usage behavior using logistic regression.

# Data Available
Voice Usage:
  Incoming / Outgoing Minutes of Usage for
  Local calls , STD calls,  ISD calls, 
  Local to other networks, STD to other networks, ISD to other networks.

Data Usage:
  2G in MB
  3G in MB
  
Payment data:
  Monthly Payment Data
  Total Bill, RentalCharge, Non Rental Charge

Subscriber information:
  Customer Type
  Account activation date
  Disconnection date (If applicable)

Customer care data
  Call to Customer Care
  Call type (Request / Query / Complaint / Feedback)
  Successive calls to customer care

# Code Details:
File: SingleView.R 
  Reads month wise input files, changes data types, calculates  derived variables.
  Aggregates records for each ID and computes mean and variance of all variables.
  Generates churn_data / validation_data csv files.

File: churn_prediction.R
  Reads churn_data / validation_data
  Applies general logistic regression, plots AUC, computes confusion matrix.


