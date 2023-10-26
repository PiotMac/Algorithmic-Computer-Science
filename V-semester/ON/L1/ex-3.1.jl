#=
LISTA 1
ZADANIE 3
Przedział [1;2]
Autor: Piotr Maciejończyk, 268425
=#
fl = Float64
step = fl(2^(-52))

# The point of start can be regulated
start = fl(0)

number = fl(1)
function check_correctness(next_or_previous, delta, this_start)
    checking_number = this_start
    for i in range(1, 10000)
        next_or_prev_number = fl(this_start + fl(delta * i))
        if (next_or_prev_number != next_or_previous(fl(checking_number)))
            return false
        end
        checking_number = next_or_previous(fl(checking_number))
    end
    return true
end

if (check_correctness(nextfloat, step, fl(1)) && check_correctness(prevfloat, -step, fl(2)))
    println("SUCCESS!")
else 
    println("ERROR!")
end

# Checking every number between 1 and 2
for i in range(start, 2^52)
    # Printing bit strings of each number
    println(bitstring(fl(number + fl(step * i))))
    # For easier presentation
    sleep(0.5)
end
