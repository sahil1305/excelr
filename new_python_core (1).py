#Operators

#Arithmetic operators

#consider
a = 10
b = 20

#Addition
a + b

#subtraction
a - b

#Multiplication
a * b

#division 
a/b

#Remove decimal
a//b


#Modules
b%a


#exponential
a**b 



#Comparision Operator

#It gives in bool values

a == b
 
a != b
 
a > b
 
a < b

a >= b

a <= b

#Assignment Operators

c = a+b
c
c += b
c
c -= b
c
c *= b
c
c /= b
c
c %= b
c
c **= b
c
c //= b
c


#bitwise operator

a = 60
b = 13

format(60,"b")
format(13,"b")

#Binary And
a & b
format(12,"b")
0
#Binary Or
a|b
format(61,"b")
#inary XOR
a ^ b

#Binary Ones Complement	
~a

#Binary Left Shift

a << 2

#Binary Right Shift

a >> 2

#Logical Operators

#Membership Operators

"i" in "Nikhil"

"l" in "nikhil"

"p" in "nikhil"

"p" not in "nikhil"

#Identity Operators
"i" is "Nikhil"
1 is 1
2 is 1
"nikhil" is "Nikhil"

1 is 0

1 is not 1

"hi hello"  is not  "hello hi"


######################## Variables #########################


#Assigning Values 

cars = 10
type(cars)
print(cars)

wt = 60.25
type(wt)

my_car = "10"
type(my_car)
my_car = 'cars2'

new = 50

bike = new


new_car = 'cars'


seats = 8

String_Name = "Python string"

complex_num = "11j+10k" 

cars
hi = "hello"
hi

colour = 'ram'
nikhil =100

my_name = nikhil

kaleem = 150
my_names = "kaleem15"

# Multiple Assignment

a = b = c = 34

print(a)

b

c

# Integer values

a,b,c,d,e = 0,2,5,7
new,old="iphone","MI"
d
a
b
e
c
print(c)


num_1 = input("hi, Enter a value =  ")
print ("you have entered ", num_1)

type(num_1)

name = input ("Enter your name = ")
print ("Hello, you entered = ", name)
print(name)
print ("hello welcome to python world ",name)



age = int(input("enter the age = "))
print("your age is: ", age)

type(age)


num_1 = float(input("Enter a 1st value = "))
num_2 = int(input("Enter a 2nd value = "))
results = num_1+num_2
print("final result", results)

type(results)

num_1 = float(input("Enter a 1st value = "))
num_2 = float(input("Enter a 2nd value = "))
results = int(num_1+num_2)
print("final result", results)




type(results)



x = eval(input("enter the 1st value = "))
y = eval(input("enter the 2nd value = "))
results = x+y
print("final results = ",results)
type(results)

try:
    x = eval(input("enter the 1st value = "))
    y = eval(input("enter the 2nd value = "))
    results = x+y
    print(results)
except:
    print("please enter a valid number")


    



try:
    x = eval(input("enter the 1st value = "))
    y = eval(input("enter the 2nd value = "))
    results = x/y
    print("final results = ", results)
except(ZeroDivisionError):
    print("please enter a non-zero value for the divisor")
except(NameError):
    print("Please enter valid number")
except(TypeError):
    print("Please enter both same type")
   

#######Strings###############


#Accesing Value in Strings   
Name = "Aditya"

print(Name)
  
print(Name[0])

print(Name[3])

print(Name[2:-5])

print(Name[-1])

print(Name[-3:-1])
  
print(Name[-6:2])  
#Update Strings    
var1 = 'Hello World!'
print ("Updated String :- ", var1 + 'Python')
print ("Updated String :- ", var1[:6] + 'Python')


# String Formating 
print("My name is %s and weight is %d kgs! my  father name is %s " % ('Anil', 70,'Ramu'))


#Ex :1 

Name = input("Enter your name: ")
Weight = eval(input("Enter your Weight: "))
(Name,Weight)

#Triple Quotes

# my name is 'nikhil' and my age is '25'

Statement = """my name is "nikhil" and my age is "25" Python Class"""

Name = "nikhil"
print(Name.capitalize())

Name.center(50)

Name.count("nikhil")

#count() method returns the number of occurrences of the substring in the given string.
string = "nikhil is trainer"
substring = "n"

count = string.count(substring)

# print count
print("The count is:", count)

#Count number of occurrences of a given substring using start and end
# define string
string = "Nikhil"
substring = "i"

# count after first 'i' and before the last 'i'
count = string.count(substring, 8, 25)

# print count
print("The count is:", count)



##Returns true if string has at least 1 character and all characters are alphanumeric and false otherwise.

Num = "thishi4";  # No space in this string
print(Num.isalnum())

Num = "this is string examplehi!!!";
Num.isalnum()

#This method returns true if all characters in the string are alphabetic and there is at least one character, false otherwise.
Num = "this";  # No space & digit in this string
Num.isalpha()

Num = "this is string example0909090!!!";
Num.isalpha()



#This method returns true if all characters in the string are digits and there is at least one character, false otherwise.

Num = "123456";  # Only digit in this string
Num.isdigit()

Num = "this is string example!!!";
Num.isdigit()

#his method returns a copy of the string in which all case-based characters have been lowercased.
Num = "THIS IS STRING EXAMPLE!!!";
Num.lower()

#his method returns a copy of the string in which all case-based characters have been Uppercase.
Num = "this is string example!!!";

Num.upper()


#The following example shows the usage of replace() method.

reply = "it is string example!!! is really a string"
print(reply.replace("is", "was"))
print(reply.replace("is", "was", 1))




#The following example shows the usage of split() method.
split1 = "Line1-abcdef \nLine2-abc \nLine4-abcd";
print(split1.split( ))
print(split1.split(' ', 1 ))

split1.split()



###############################   List   ################################

list1 = ['Nikhil', 'Excelr', 2013, 2018]
list2 = [1, 2, 3, 4, 5 ]
list3 = ["a", "b", "c", "d"]



python_class = ["Nikhil", "Aditya", "Divya"]


list1 = ['Nikhil', 'Excelr', 2013, 2018];

print(list1[0])


list2 = [1, 2, 3, 4, 5, 6, 7 ]

print(list2[1:5])




list1 = ['Nikhil', 'Excelr', 2013, 2018]
print(list1[2])


list1[2] = 8055
print(list1)

list1[0] = "Divya"


list1 = ['Nikhil', 'Excelr', 2013, 2018]
print(list1)
del(list1[2])

print(list1)


# Append

aList = [123, 'xyz', 'zara', 'abc'];
aList.append( 2009 );
print(aList)

#Pop

print (aList.pop())
print (aList.pop(2))


#Insert

aList.insert( 1, 2009)
print (aList)

aList.insert(2,"excelr")
#Extend

aList = [123, 'xyz', 'tommy', 'abc', 123];
bList = [2009, 'beneli'];

bList.extend(aList)

print(aList)

#Reverse

aList.reverse();
print(aList)


#Sort


blist = [8,99,45,33]
blist.sort(reverse=True);
print(blist)

#count

aList = [123, 'xyz', 'zara', 'abc', 123, "zara"];
print(aList.count("zara"))


##################################### Tuples ####################################


## Create a tuple dataset
tup1 = ('Street triple','Detona','Beneli', 8055)
tup2 = (1, 2, 3, 4, 5 )
tup3 = ("a", "b", "c", "d")

### Create a empty tuple 
tup1 = ();

#Create a single tuple
tup1 = (50,);

#Accessing Values in Tuples
tup1 = ('Street triple','Detona','Beneli', 8055);
print(tup1[0]);

tup2 = (1, 2, 3, 4, 5, 6, 7 );
print(tup2[1:5]);

#Updating Tuples
tup1 = (12, 34.56);
tup2 = ('abc', 'xyz');

# So,create a new tuple as follows
tup1 = tup1 + tup2;
print(tup1);

#Delete Tuple Elements
tup = ('Street triple','Detona','Beneli', 8055)
print (tup)
del(tup)
print ("After deleting tup : ")
print(tup)

#Basic Tuples Operations

#To know length of the tuple 
tup = (1,2,3,'Nikhil','Python')
len(tup)

#To add two elements
tup2 =(4,5,6) 

tup3 = tup+tup2

tup3 = ('Hi!',)
tup*3
tup3*3


# Membership
3 in (1, 2, 3)



# Max and min in tuple
tuple1 = (456, 700, 200)
print(max(tuple1))

print(min(tuple1))



############################## Dictionary #################################


#Accessing Values in Dictionary
dict1 = {'Name': 'Nikhil', 'Age': 25, 'bike': 'Beneli'}
print(dict1)
print(dict1['Name'])
print(dict1['Age'])   
print(dict1['bike'])


##Updating Dictionary
dict1 = {'Name': 'Nikhil', 'Age': 25, 'bike': 'Beneli'}
dict1['1'] = 8 # update existing entry
dict1['School'] = "DPS School"# Add new entry
dict1['Sal'] = 50000

print(dict1['Age'])
print(dict1['School'])


#Delete Dictionary Elements
dict1 = {'Name': 'Nikhil', 'Age': 25, 'bike': 'Beneli'}
del(dict1['Name']) # remove entry with key 'Name'
dict1.clear()    # remove all entries in dict
del(dict1)        # delete entire dictionary

print(dict1['Age'])
print(dict1['School'])



############################# Decision Making ####################################


is_male = False

if is_male:
    print("your male")
else:
    print("your female")
    

is_male = True 
is_tall = False 

if is_male and is_tall:              ## and & not operator 
    print("you are tall male")
elif is_male and not(is_tall):
    print("you are short male")
elif not(is_male) and is_tall:
    print("you are not male but tall")
else:
    print("you are not male and not tall")
    
    
if not(is_male) or is_tall:
  print(" you are male or a tall or both") ## or operator 
  

#### define function ###
  
def hello(name):                  #simple function 
    print("hello",name)

hello('nikhil')

def hello(name,age):
    print("hello hi",name,age)

hello("nikhil",25)



def add(x,y,z):                    #add two numbers using return value 
    return (x-y+z)

add(10,20,50)


def cube(num):                   #cube of n value 
    return (num*num*num)

cube(3)


def hello_func():                # not defined any value but just defined function 
    pass



#1000 of code
print("Hello Function. nikhil") #10
print("Hello Function.nikhil")#14
print("Hello Function.nikhil")#40
print("Hello Function.nikhil")#67

def hello_func():
    print("Hello Function.nikhil!")

hello_func()#10
hello_func()#14
hello_func()#40
hello_func()#67



def hello_func(greeting):
    return '{} hola'.format(greeting)

print(hello_func('hi'))


def hello_fun(greeting,name = "you"):
    return '{},{} enjoy'.format(greeting,name)

hello_fun()

def hello_func(greeting, name = 'you'):
    return '{},{}'.format(greeting,name)

print(hello_func('Hi', name = 'Nikhil'))


def staff_info(*args,**kwargs):
    print(args)
    print(kwargs)
    
staff_info('sales','marketing','python','classes',name1='divya',name2 = 'aditya', name = 'nikhil', age =25)


##########
courses = ["sales","marketing"]
info = {"name":"nikhil","age":25}
staff_info(*courses,**info)
#Operators

#Arithmetic operators

#consider
a = 10
b = 20

#Addition
a + b

#subtraction
a - b

#Multiplication
a * b

#division 
a/b


#Modules
b%a


#exponential
a**b 


#Remove decimal
a//b


#Comparision Operator

#It gives in bool values

a == b
 
a != b
 
a > b
 
a < b

a >= b

a <= b

#Assignment Operators

c = a+b
c
c += b

c -= b

c *= b

c /= b

c %= b

c **= b

c //= b
c

#bitwise operator

a = 60
b = 13

format(60,"b")
format(13,"b")

#Binary And
a & b
format(12,"b")

#Binary Or
a|b
format(61,"b")
#Binary XOR
a ^ b

#Binary Ones Complement	
~a

#Binary Left Shift

a << 2

#Binary Right Shift

a >> 2

#Logical Operators

#Membership Operators

"i" in "Nikhil"

"l" in "nikhil"

"p" in "nikhil"

"p" not in "nikhil"

#Identity Operators

1 is 1
"nikhil" is "nikhil"
[1,2,3] is [1,4,5]
1 is 0

1 is not 1

"hi hello" is  "hello hi"


######################## Variables #########################


#Assigning Values 

cars = 10
print(cars)

cars

seats = 8

colour = "string"

nikhil =100

my_name = nikhil



# Multiple Assignment

a = b = c = 2

print(a)

b

c

# Integer values

a,b,c,d,e = 0,2,5,7,8

a
b
e

num_1 = input("Enter a value ")
print ("you have entered ", num_1)

type(num_1)

name = input ("Enter your name = ")
print ("Hello, you entered = ", name)
print(name)
print ("hello welcome to python world ",name)



num_1 = int(input("enter the value = "))
print("you have entered", num_1)

type(num_1)


num_1 = float(input("Enter a 1st value = "))
num_2 = int(input("Enter a 2nd value = "))
results = num_1+num_2
print("final result", results)

type(results)

num_1 = float(input("Enter a 1st value = "))
num_2 = float(input("Enter a 2nd value = "))
results = num_1+num_2
print("final result", results)




type(results)



x = eval(input("enter the 1st value = "))
y = eval(input("enter the 2nd value = "))
results = x+y
print("final results = ",results)
type(results)

try:
    x = eval(input("enter the 1st value = "))
    y = eval(input("enter the 2nd value = "))
    results = x+y
except:
    print("please enter a valid number")




try:
    x = eval(input("enter the 1st value = "))
    y = eval(input("enter the 2nd value = "))
    results = x/y
    print("final results = ", results)
except(ZeroDivisionError,NameError,KeyboardInterrupt,SyntaxError):
    print("please enter a non-zero value for the divisor")
    print("Please enter valid number")
    print("Thank you and goodbye")
    print("Valid value") 
finally:
    print("Done")
    
#Assertion

def get_age(age):
    assert age > 0, " Please provide proper age"   
    print("ok, your age is: ", age)
    
get_age(-1)    
#######Strings###############


#Accesing Value in Strings   
Name = "Aditya"
  
print(Name[0])

print(Name[3])

print(Name[1:4])

print(Name[-1])

print(Name[-3:-1])
    
#Update Strings    
var1 = 'Hello World!  '
print ("Updated String :- ", var1 + 'Python')
print ("Updated String :- ", var1[:6] + 'Python')


# String Formating 
print("My name is %s and weight is %d kgs!" % ('Nikhil', 20))


#Ex :1 

Name = input("Enter your name: ")
Weight = eval(input("Enter your Weight: "))
print("My name is %s and my Weight is %d"%(Name,Weight))

type(Age)

#Triple Quotes


# my name is 'nikhil' and my age is '25'

Statement = """my name is 'nikhil' and my age is '25'"""

Name = "nikhil"
type(Name)
print(Name.capitalize())

Name.center(50)

Name.count("nikhil")

#count() method returns the number of occurrences of the substring in the given string.
string = "nikhil is trainer"
substring = "i"

count = string.count(substring)

# print count
print("The count is:", count)


#Count number of occurrences of a given substring using start and end
# define string
string = "nikhil is trainer"
substring = "i"

# count after first 'i' and before the last 'i'
count = string.count(substring, 8, 25)

# print count
print("The count is:", count)



##Returns true if string has at least 1 character and all characters are alphanumeric and false otherwise.

Num = 'thishi';  # No space in this string
print(Num.isalnum())

Num = "this is string examplehi!!!";
Num.isalnum()

#This method returns true if all characters in the string are alphabetic and there is at least one character, false otherwise.
Num = "this is";  # No space & digit in this string
Num.isalpha()

Num = "this is string example0909090!!!";
Num.isalpha()



#This method returns true if all characters in the string are digits and there is at least one character, false otherwise.

Num = "123456";  # Only digit in this string
Num.isdigit()

Num = "this is string example!!!";
Num.isdigit()

#his method returns a copy of the string in which all case-based characters have been lowercased.
Num = "THIS IS STRING EXAMPLE!!!";
Num.lower()

#his method returns a copy of the string in which all case-based characters have been Uppercase.
Num = "this is string example!!!";

Num.upper()


#The following example shows the usage of replace() method.

reply = "it is string example!!! is really a string"
print(reply.replace("is", "was"))
print(reply.replace("is", "was", 2))




#The following example shows the usage of split() method.
split1 = "Line1-abcdef \nLine2-abc \nLine4-abcd";
print(split1.split( ))
print(split1.split(' ', 1 ))

split1.split()




###############################   List   ################################

list1 = ['Nikhil', 'Excelr', 2013, 2018]
list2 = [1, 2, 3, 4, 5 ]
list3 = ["a", "b", "c", "d"]



python_class = ["Nikhil", "Aditya", "Divya"]


list1 = ['Nikhil', 'Excelr', 2013, 2018];

print(list1[0])


list2 = [1, 2, 3, 4, 5, 6, 7 ];

print(list2[1:5])




list1 = ['Nikhil', 'Excelr', 2013, 2018];
print(list1[2])


list1[2] = 8055;
print(list1)

list1[0] = "Divya";


list1 = ['Nikhil', 'Excelr', 2013, 2018];
print(list1)
del(list1[2]);

print(list1)


# Append

aList = [123, 'xyz', 'zara', 'abc'];
aList.append( 2009 );
print(aList)

#Pop

print (aList.pop())
print (aList.pop(2))


#Insert

aList.insert( 3, 2009)
print (aList)

aList.insert(2,"excelr")
#Extend

aList = [123, 'xyz', 'tommy', 'abc', 123];
bList = [2009, 'beneli'];
print(cab) 
aList.extend(bList)
bList.extend(aList)
#Reverse

aList.reverse();
print(aList)


#Sort

aList = ['xyz', 'Tommy', 'abc', 'xyz'];
aList.sort();
print(aList)

blist = [8,99,45,33]
blist.sort();
print(blist)

#count

aList = [123, 'xyz', 'zara', 'abc', 123, "a"];
print(aList.count("zara"))

##################################### Tuples ####################################


## Create a tuple dataset
tup1 = ('Street triple','Detona','Beneli', 8055);
tup2 = (1, 2, 3, 4, 5 );


### Create a empty tuple 
tup1 = ();

#Create a single tuple
tup1 = (50,);

#Accessing Values in Tuples
tup1 = ('Street triple','Detona','Beneli', 8055);
print(tup1[0]);

tup2 = (1, 2, 3, 4, 5, 6, 7 );
print(tup2[1:5]);

#Updating Tuples
tup1 = (12, 34.56);
tup2 = ('abc', 'xyz');

# So,create a new tuple as follows
tup1 = tup1 + tup2;
print(tup1);

#Delete Tuple Elements
tup = ('Street triple','Detona','Beneli', 8055);
print tup;
del tup;
print "After deleting tup : ";
print tup;

#Basic Tuples Operations

#To know length of the tuple 
tup = (1,2,3)
len(tup)

#To add two elements
tup2 =(4,5,6) 

tup+tup2

tup3 = ('Hi!',)
tup*3
tup3*3


# Membership
3 in (1, 2, 3)



# Max and min in tuple
tuple1 = (456, 700, 200)
print(max(tuple1))

print(min(tuple1))



############################## Dictionary #################################


#Accessing Values in Dictionary
dict1 = {'Name': 'Nikhil', 'Age': 25, 'bike': 'Beneli'}
print(dict1)
print(dict1['Name'])
print(dict1['Age'])   



##Updating Dictionary
dict1 = {'Name': 'Nikhil', 'Age': 25, 'bike': 'Beneli'}
dict1['Age'] = 8; # update existing entry
dict1['School'] = "DPS School"; # Add new entry
dict1['sal'] = 50000


print(dict1['Age'])
print(dict1['School'])


#Delete Dictionary Elements
dict1 = {'Name': 'Nikhil', 'Age': 25, 'bike': 'Beneli'}
del(dict1['Name']); # remove entry with key 'Name'
dict1.clear();     # remove all entries in dict
del(dict1) ;        # delete entire dictionary

print(dict1['Age'])
print(dict1['School'])



############################# Decision Making ####################################


is_male = True

if is_male:
    print("your male")
else:
    print("your female")



is_male = True
is_tall = False 

if is_male and is_tall:              ## and & not operator 
    print("you are tall male")
elif is_male and not(is_tall):
    print("you are short male")
elif not(is_male) and is_tall:
    print("you are not male but tall")
else:
    print("you are not male and not tall")
    
    
if is_male or is_tall:
  print(" you are male or a tall or both") ## or operator 
  
  

### Nested if Statement

score = 50
money = 6000
age = 65

if score > 100:
    print("You got good points")
    
    if money >= 5000:
        print("you win")
        
        if age >= 30:
            print("You win in middle age")
        else:
            print("You are win in young age")
    else:
        print("you have a high points but you do not have enough money")
        
else:
    print("your loser")
 

#Ex:2
    
name = "human"
animalName = "dog"

if name == "animal":
    print("Name Entered is Animal")
    if animalName == "dog":
        print("valid Animal")
    else:
        print("animalName invalid")
else:
    print("the name entered is not valied")
    print ("your entered name is not a animal")
    


  

#### define function ###
  
def hello(name,age,sal):                  #simple function 
    print("hi",name,"your age:",age,"your salary:",sal)

  hello("Nikhil",25,50000)

hello('nikhil')
hello("shiva", 25)

def hello(name,age):
    print("hello hi",name,age)

hello("nikhil",25)



def add(x,y,z):                    #add two numbers using return value 
    return (x-y+z)

add(10,20,50)

add(10,50,100)

def cube(num):                   #cube of n value 
    return (num*num*num)

cube(3)


def hello_func():                # not defined any value but just defined function 
    pass



#1000 of code
print("Hello Function. nikhil") #10
print("Hello Function.nikhil")#14
print("Hello Function.nikhil")#40
print("Hello Function.nikhil")#67

def hello_func():
    print("Hello Function.nikhil!")

hello_func()#10
hello_func()#14
hello_func()#40
hello_func()#67



def hello_func(greeting,name):
    return '{}{}'.format(greeting,name)

print(hello_func('hi',"Nikhil"))


def hello_fun(greeting,name = "you"):
    return '{},{} enjoy'.format(greeting,name)

hello_fun("hi","nikhil")

def hello_func(greeting, name = 'you'):
    return '{},{}'.format(greeting,name)

print(hello_func('Hi', name = 'Nikhil'))


def staff_info(*args,**kwargs):
    print(args)
    print(kwargs)
    
staff_info('sales','marketing','python','classes','today','hi',name1='divya',name2 = 'aditya', name = 'nikhil', age =25)


##########
courses = ["sales","marketing"]
info = {"name":"nikhil","age":25}
staff_info(*courses,**info)




    
    
## Define a function with conditional statement
    
def max_num(num1,num2,num3):
    
    if num1 >= num2 and num1 >= num3:
        
        return num1
    
    elif num2 >= num1 and num2 >= num3:
        return num2

    else:
        return num3
    
print (max_num(400,60,1150))


############# Loops #####################

# While Loop

#Ex: 1

count = 0

while count < 20:
    print("Digit: ", count)
    count = count + 1
    
print("Thank you")

#Ex: 2

import random 

n = 20

random_number = int(n * random.random())

guess = 0

while guess != random_number:
    guess = int(input("New Number: "))
    if guess > 0:
        if guess > random_number:
            print("number is too large")
        elif guess < random_number:
            print("number is too small")
    else:
        print("sorry that you are giveup!")
        break
else:
    print("Congratulations. YOU WON!")
    
    
            
                
# for Loop
    

#Ex:1
    
snacks = ['pizza','burger','shawarma','franky']

for snack in snacks:
    print("current snack: ", snack)
    
print("Good day!")
     



#Ex:2 

num = int(input("number: "))
factorial =1 

if num < 0:
    print("must be positive")
elif num == 0:
    print("factorial = 1")
else:
    for i in range(1,num+1):
        factorial = factorial * i
    print("factorial =  " ,factorial)
        
    
    
#Nested Loops
    

                
print('Welcome to Northen Frock Bank ATM')
restart=('Y')
chances = 3
balance = 67.14 
while chances >= 0:
    pin = int(input('Please Enter You 4 Digit Pin: '))
    if pin == (1234):
        print('You entered you pin Correctly\n')
        while restart not in ('n','NO','no','N'):
            print('Please Press 1 For Your Balance\n')
            print('Please Press 2 To Make a Withdrawl\n')
            print('Please Press 3 To Pay in\n')
            print('Please Press 4 To Return Card\n')
            option = int(input('What Would you like to choose?: '))
            if option == 1:
                print('Your Balance is Â£',balance,'\n')
                restart = input('Would You you like to go back? ')
                if restart in ('n','NO','no','N'):
                    print('Thank You')
                    break
            elif option == 2:
                option2 = ('y')
                withdrawl = float(input('How Much Would you like to withdraw? \nÂ£10/Â£20/Â£40/Â£60/Â£80/Â£100 for other enter 1: '))
                if withdrawl in [10, 20, 40, 60, 80, 100]:
                    balance = balance - withdrawl
                    print ('\nYour Balance is now Â£',balance)
                    restart = input('Would You you like to go back? ')
                    if restart in ('n','NO','no','N'):
                        print('Thank You')
                        break
                elif withdrawl != [10, 20, 40, 60, 80, 100]:
                    print('Invalid Amount, Please Re-try\n')
                    restart = ('y')
                elif withdrawl == 1:
                    withdrawl = float(input('Please Enter Desired amount:'))    

            elif option == 3:
                Pay_in = float(input('How Much Would You Like To Pay In? '))
                balance = balance + Pay_in
                print ('\nYour Balance is now Â£',balance)
                restart = input('Would You you like to go back? ')
                if restart in ('n','NO','no','N'):
                    print('Thank You')
                    break
            elif option == 4:
                print('Please wait whilst your card is Returned...\n')
                print('Thank you for you service')
                break
            else:
                print('Please Enter a correct number. \n')
                restart = ('y')
    elif pin != ('1234'):
        print('Incorrect Password')
        chances = chances - 1
        if chances == 0:
            print('\nNo more tries')
            break            
            
                      ##0
        
#Nested for loop
            
for i in range(0,3):
    for j in range(0,3):
        print(i,j)
   
# =============================================================================

#        0
#       000
#      00000
#     0000000
#    000000000
#   00000000000
#  0000000000000
        
# =============================================================================

my_pri = 20

for i in range(0,my_pri):
    for j in range(0,my_pri-i):
        print(" ",end="")
    for k in range(0,2*i+1):
        print("0",end="")
    print("")

for i in range(0, 7):
    for j in range(0, 7-i):
        print(" ",end ="")
    for k in range (0,2*i+1):
        print("0",end ="")
    print("")
    
    

#Nested while in for 

travelling  = input("yes or no" )

while travelling == 'yes':
    
    num = int(input("number of people travelling: " ))
    
    for num in range(1,num+1):
        name = input("Name: ")
        
        age = input("Age: ")
        
        sex= input("Male or Female: ")
        
        print(name)
        
        print(age)
        
        print(sex)
        
    travelling = input ("Oops! forgot someone")









################ Modules ######################

#Ex:1

import math

math.sqrt(16)

math.pow(2,5)

dir(math)

math.

#Ex:2

import calendar 

cal = calendar.month(2018,1)

print(cal)


calendar.isleap(2018)

calendar.isleap(2020)

calendar.isleap()

dir(calendar)

#to give path for the module
import sys
sys.path.append("F:\Python Classes")

#call module and use functions
import area_functions as f

area = f.calculate_square_area(5)
print(area)

area = f.calculate_triangle_area(5,10)
print(area)


# how to wrte a file

f = open("D:\\python_classes_code\\ funny.txt","a")
f.write("I love python \n I love Java")
f.close()


## Regular expressions

import re

Nameage = '''Nikhil is 25 and Pavan is 28 and Kaleem is 35 and Ganesh is 16'''

age = re.findall(r'\d{1,3}',Nameage)
names = re.findall(r'[A-Z][a-z]*',Nameage)

agedict = {}

x = 0

for eachname in names:
     agedict[eachname] = age[x]
     x +=1
     
print(agedict)


#Ex:2

import re

allname = re.findall("name","My name is Nikhil and what is your name")

for name in allname:
    print(name)
    
    
#Ex:3 find iterator
        
import re

allname = "My name is Nikhil and what is your name"   

for i in re.finditer("name",allname):
    loctup = i.span()
    print(loctup)
    
#Ex: 4 Find match pattern
    
import re

names = "Aam,Bam,Sam,Ram,Mam,Jam"

allnames = re.findall("[A-Z]am",names) 

for i in allnames:
    
    print(i)
    

#Replace string 

import re 

names = "Sam,Ram,Mam,Jam"

regex =  re.compile("[R]am")  

names = regex.sub("Rama",names)

print(names)


## dividing num,alpha,special char
import re

re.sub("[A-Za-z]",",","ada 1231zxdq #@$@zxfsd312")
       
re.sub("[^A-Za-z]"," ","ada 1231zxdq #@$@zxfsd312")
8
       
## Remove white space

randstr = ''' 
My 
name 
is 
nikhil
'''
print(randstr)

regex = re.compile("\n")

randstr = regex.sub("",randstr)

print(randstr)

## Number

import re

randstr = "12345"

print(len(re.findall("\d",randstr)))

print(re.search(r"\d{3}",randstr))


num = "123 1234 12345 123456 1234567"

print(len(re.findall("\d{5,7}",num)))

## Phone number verification 

import re

#\w [a-zA-Z0-9]
#\w [^a-zA-Z0-9]


phn = "4669-5549-AA1212hhwjjwkq3"

if re.search("\d{3}-\d{3}-\w{6}",phn):
    
    print("it is a Pan No.")
    
else:
    print("its not a Pan No.")
    


#Email Match 
    
import re

email = "nk@hyd.com md@.com @llo.com j@com"

print("EmailMatches:", (re.findall("[\w._%+-]{1,20}@[\w.-]{2,20}.[A-Za-z]{2,3}",email)))  

#Web

import urllib.request
from re import findall

url = "https://www.bluecorona.com/blog/where-to-list-your-phone-number-on-your-website"

response = urllib.request.urlopen(url)

html = response .read()
htmlstr = html.decode()

pdata = findall("\(\d{3}\)-\d{3}-\d{4}",htmlstr)

for item in pdata:
    print(item)
    
    
#Inheritance
#What is Inheritance?
#Inheritance is a powerful feature in object oriented programming.
#Single level, Multi-level, Multiple Inheritance



x = 10

print(type(x))

class bike:
    
    def __init__(self,MPG,Speed,length,b):
        self.MPG = MPG
        self.Speed = Speed
        self.length = length
        self.b = b
    def get_MPG(self):
        print(self.MPG)
    
    def area(self):
        return self.length*self.b
    
    def config(self):
        print("1000cc,abs")
    def speed(self):
        print("Top Speed is 250KMPS") 
        
comp1 = bike()

bike.config(comp1) 
comp1.config()


class food(object):
    sweets = False
    def branch(self):
        if not self.sweets:
         print("branch")   

       
x = food()

x.branch()
    



class A:
    def product1(self):
        print("print product name1: ")
    
    
    def product2(self):
        print("print product name2: ")
        
    
name_a = A() 
     
print(id(name_a))


class B(A):
    def product3(self):
        print("print product name3: ")
    def product4(self):
        print("print product name4: ")
        
name_b = B()
print(id(name_b))
name_b.

class C():
    def product5(self):
        print("print product name5: ")
    def product6(self):
        print("print product name6: ")
        
name_c = C()

name_c.

class D(A,C):
    def product7(self):
        print("Print product name7: ")
    def product8(self):
        print("Print Product name8: ")

name_d = D()



def area_Rec(L,b):
    return L*b


area_Rec(2,3)


Rec = lambda L,B : L*B

Rec(2,3)

Sq = lambda S : S*S

Sq(2)


# Filter, Map, Reduse  

num = [2,3,4,5,6,7,8]

def even(N):
    return N%2==0


even(num)

#Filter

even = list(filter(lambda n : n%2==0, num))


#Map

add = list(map(lambda n : n+2, even))


#Reduce 

from functools import reduce

final  = reduce(lambda m,n : m+n, add)
print(final)
