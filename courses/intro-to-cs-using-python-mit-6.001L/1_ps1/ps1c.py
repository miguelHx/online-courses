## 6.100A PSet 1: Part C
## Name:
## Time Spent:
## Collaborators:

##############################################
## Get user input for initial_deposit below ##
##############################################
initial_deposit = float(input('Enter the initial deposit: '))


#########################################################################
## Initialize other variables you need (if any) for your program below ##
#########################################################################
house_cost = 800000
down_payment_rate = 0.25
down_payment_amount = house_cost * down_payment_rate
months = 36
epsilon = 100


##################################################################################################
## Determine the lowest rate of return needed to get the down payment for your dream home below ##
##################################################################################################

low = 0.0
high = 1.0
r = (high + low) / 2.0
guess = (initial_deposit * (1 + (r / 12.0)) ** months)
steps = 1
while abs(down_payment_amount - guess) > epsilon:
    if guess > down_payment_amount:
        high = r
    else:
        low = r
    r = (high + low) / 2.0
    if r == 1.0:
        r = None
        steps = 0
        break
    guess = (initial_deposit * (1 + (r / 12.0)) ** months)
    steps += 1

print('Best savings rate:', r, '[or very close to this number]')
print('Steps in bisection search:', steps, '[or very close to this number]')