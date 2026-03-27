## 6.100A PSet 1: Part B
## Name:
## Time Spent:
## Collaborators:

##########################################################################################
## Get user input for yearly_salary, portion_saved, cost_of_dream_home, semi_annual_raise below ##
##########################################################################################

yearly_salary = float(input('enter yearly salary: '))
portion_saved = float(input('enter % of salary to save, as a decimal: '))
cost_of_dream_home = float(input('enter cost of dream home: '))
semi_annual_raise = float(input('enter semi-annual raise, as a decimal: '))


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
    if months % 6 == 0:
        yearly_salary += yearly_salary * semi_annual_raise
        monthly_saved = (yearly_salary / 12.0) * portion_saved

print('number of months: ', months)
