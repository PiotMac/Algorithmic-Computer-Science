#=
LISTA 1
ZADANIE 3
Przedział [2;4]
Autor: Piotr Maciejończyk, 268425
=#
fl = Float64
# The step is multiplied by 2
# in comparison to [1;2]
step = fl(2^(-51))
start = fl(0)
# It starts from 2
number = fl(2)

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

if (check_correctness(nextfloat, step, fl(2)) && check_correctness(prevfloat, -step, fl(4)))
    println("SUCCESS!")
else 
    println("ERROR!")
end

for i in range(start, 2^52)
    println(bitstring(fl(number + fl(step * i))))
    sleep(0.5)
end