# programa que devuelve error al ejecutar typeof (la asignacion parece global)
a = 2
if a == 1 
    input = true
elsif a == 2
    input = 1
else
    input = "a"
end
puts typeof(input)
if input = true && input.is_a?(Int32)
    puts input
    puts typeof(input) 
else
    puts input
end
#programa que ejecuta la primera expresion del and caundo no deberia
a = 2
if a == 1 
    input = true
elsif a == 2
    input = 1
else
    input = "a"
end
puts typeof(input)
if input = true && input.is_a?(Int32)
    puts input
    puts typeof(input) 
else
    puts input
end