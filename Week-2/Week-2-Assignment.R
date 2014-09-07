#Jayavardhan Daripineni

#### Question 1####
queue <- c('James', 'Mary', 'Steve', 'Alex', 'Patricia')
queue <- c(queue, 'Harold')  #Arrival of Harold
queue <- queue[-1] #1st person checked out
queue <- c(queue[1],'Pam',queue[2:5]) #Pam arrived in front of Steve
queue <- c(queue[which(queue!='Harold')]) #Harold left the queue
queue <- c(queue[which(queue!='Alex')]) #Alex left the queue. We don't know the index of Alex
which(queue %in% c('Patricia')) #Patricia is 4th or last in the queue
length(queue) #Length = 4

#### Question 2####
#Already implemented discriminant dependency in quiz 
f = function(a,b,c) {
  if (b^2 >= 4*a*c){
    return(c((-1*b+sqrt(b^2-4*a*c))/(2*a),(-1*b-sqrt(b^2-4*a*c))/(2*a)))
  }
  else return ("No real roots as the determinant is -ve.")
}
#Sample 1
f(1,2,0)
#Result: [1]  0 -2
#Sample 2
f(2,5,6)
#Result: [1] "No real roots as the determinant is -ve."

#### Question 3####
div <- c(1:1000)
div3 <- div %% 3 #Save remainders when divided by 3. A non zero implies there's a remainder and is not divisible
length(div3[which(div3!=0)]) #667 numbers are not divisible by 3. 

div7 <- div %% 7 #Save remainders when divided by 3
length(div7[which(div7!=0)]) #858 numbers are not divisible by 7

div11 <- div %% 11 #Save remainders when divided by 3
length(div11[which(div11!=0)]) #910 numbers are not divisible by 11

#### Question 4####
f <- function (f,g,h){
  #Check if a^2 + b^2 = c^2 where c is the largest value.
  if (sort(c(f,g,h))[1]^2 + sort(c(f,g,h))[2]^2 == sort(c(f,g,h))[3]^2){ return ("Yes, Pythagorean triple")}
  else {return ("Nope.! Not a Pythagorean triple.")}
}
#Sample 1
f(4,3,5)
#Result: [1] "Yes, Pythagorean triple"
#Sample 2
f(4,6,5)
#Result: "Nope.! Not a Pythagorean triple."

