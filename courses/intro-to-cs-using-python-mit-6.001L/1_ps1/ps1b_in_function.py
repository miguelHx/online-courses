def part_b(yearly_salary, portion_saved, cost_of_dream_home, semi_annual_raise):
	#########################################################################
	
	
	###############################################################################################
	## Determine how many months it would take to get the down payment for your dream home below ## 
	###############################################################################################
	
	
	yearly_salary = float(input('enter yearly salary: '))
	portion_saved = float(input('enter % of salary to save, as a decimal: '))
	cost_of_dream_home = float(input('enter cost of dream home: '))
	semi_annual_raise = float(input('enter semi-annual raise, as a decimal: '))
	
	portion_down_payment = 0.25
	r = 0.05
	down_payment_amount = cost_of_dream_home * portion_down_payment
	
	first_month_saved = (yearly_salary / 12.0) * portion_saved
	amt_saved = 0
	monthly_return = first_month_saved * (r / 12)
	
	months = 0
	while amt_saved <= down_payment_amount:
	    amt_saved += first_month_saved + monthly_return
	    monthly_return = amt_saved * (r / 12)
	    months += 1
	    if months % 6 == 0:
	        yearly_salary += yearly_salary * semi_annual_raise
	        first_month_saved = (yearly_salary / 12.0) * portion_saved
	print('number of months: ', months)
	return months