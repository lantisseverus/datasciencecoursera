h = input('請輸入身高(公尺): ')
m = input('請輸入體重(公斤): ')
h = float(h)
m = float(m)
bmi = m / h**2
print(bmi)
if bmi < 18.5:
	print('過輕')
elif bmi < 24 and bmi >= 18.5:
	print('正常')
else:
	print('過重')