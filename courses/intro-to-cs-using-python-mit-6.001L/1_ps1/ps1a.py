## 6.100A PSet 1: Part A
## Name:
## Time Spent:
## Collaborators:

##################################################################################
## Get user input for yearly_salary, portion_saved and cost_of_dream_home below ##
##################################################################################
yearly_salary = float(input('Enter your yearly salary: '))
portion_saved = float(input('Enter the percent of your salary to save, as a decimal: '))
cost_of_dream_home = float(input('Enter the cost of your dream home: '))

#########################################################################
## Initialize other variables you need (if any) for your program below ##
#########################################################################

portion_down_payment = 0.25
amount_saved = 0
r = 0.05

monthly_rate = r / 12
monthly_saved = yearly_salary / 12 * portion_saved

# monthly_return = amount_saved * (r/12)

cost_of_down_payment = cost_of_dream_home * portion_down_payment


###############################################################################################
## Determine how many months it would take to get the down payment for your dream home below ## 
###############################################################################################


months = 0

while amount_saved < cost_of_down_payment:
    monthly_return = amount_saved * monthly_rate
    amount_saved = amount_saved + monthly_saved + monthly_return
    months += 1


print('number of months: ', months)



