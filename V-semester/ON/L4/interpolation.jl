#=
LISTA 4
ZADANIE 1, 2, 3, 4
Autor: Piotr Maciejończyk, 268425
=#

module Interpolation

using Plots

# Funkcja obliczająca ilorazy różnicowe
#
# # Argumenty
#
# - `x`: wektor długości n zawierający węzły x_0, ..., x_n-1
#        x[1] = x_0, ..., x[n] = x_n-1
# - `f`: wektor długości n zawierający wartości interpolowanej
#        funkcji w węzłach f(x_0), ..., f(x_n-1)
#
# # Zwraca
#
# - `fx`: wektor długości n zawierający obliczone ilorazy różnicowe
#         fx[1]=f[x_0],
#         fx[2]=f[x_0, x_1], ..., fx[n]=f[x_0, ..., x_n−1].
#
function ilorazyRoznicowe(x::Vector{Float64}, f::Vector{Float64})
    x_length = length(x)
    y_length = length(f)

    if (x_length != y_length || x_length == 0)
        throw(DimensionMismatch("Podane wektory nie są tej samej wielkości lub są rozmiaru 0..."))
    end

    n = x_length

    fx = zeros(n)

    # Obliczanie wartości w pierwszej kolumnie tablicy trójkątnej
    for i in 1:n
        fx[i] = f[i]
    end

    #
    #fx[n] = (fx[n] - fx[n-1]) / (x[n] - x[n - 1])
    #fx[n-1] = (fx[n-1] - fx[n-2]) / (x[n-1] - x[n - 2])

    #fx[3] = (fx[3] - fx[2]) / (x[3] - x[2])
    #fx[2] = (fx[2] - fx[1]) / (x[2] - x[1])

    # Iterowanie po kolumnach tablicy trójkątnej
    for j in 2:n
        # Obliczanie ilorazów różnicowych w j-tej kolumnie
        for k in n:-1:j
            fx[k] = (fx[k] - fx[k - 1]) / (x[k] - x[k - j + 1])
        end
    end

    return fx
end


# Funkcja obliczająca wartość wielomianu interpolacyjnego stopnia n w postaci Newtona
# w punkcie x = t za pomocą uogólnionego algorytmu Hornera
#
# # Argumenty
#
# - `x`: wektor długości n zawierający węzły x_0, ..., x_n-1
#        x[1] = x_0, ..., x[n] = x_n-1
# - `fx`: wektor długości n zawierający ilorazy różnicowe
#         fx[1]=f[x_0],
#         fx[2]=f[x_0, x_1], ..., fx[n]=f[x_0, ..., x_n−1].
# - `t`: punkt, w którym należy obliczyć wartość wielomianu
#
# # Zwraca
#
# - `nt`: wartość wielomianu w punkcie t
#
function warNewton(x::Vector{Float64}, fx::Vector{Float64}, t::Float64)
    x_length = length(x)
    y_length = length(fx)

    if (x_length != y_length || x_length == 0)
        throw(DimensionMismatch("Podane wektory nie są tej samej wielkości lub są rozmiaru 0..."))
    end

    n = x_length

    # Trzeba obliczyć wielomian o postaci:
    # p(t) = f[x_0] + (t - x_0) * f[x_0, x_1] + (t - x_0)(t - x_1) * f[x_0, x_1, x_2] + ... + (t - x_0)...(t - x_n-1) * f[x_0, ..., x_n]

    nt = fx[n]
    for i in (n-1):-1:1
        nt = nt * (t - x[i]) + fx[i]
    end

    return nt
end


# Funkcja obliczająca dla wielomianu w postaci Newtona współczynniki jego postaci naturalnej
#
# # Argumenty
#
# - `x`: wektor długości n zawierający węzły x_0, ..., x_n-1
#        x[1] = x_0, ..., x[n] = x_n-1
# - `fx`: wektor długości n zawierający ilorazy różnicowe
#         fx[1]=f[x_0],
#         fx[2]=f[x_0, x_1], ..., fx[n]=f[x_0, ..., x_n−1].
#
# # Zwraca
#
# - `a`: wektor długości n zawierający obliczone współczynniki postaci naturalnej
#        a[1]=a0,
#        a[2]=a1, ..., a[n]=a_n−1.
#
function naturalna(x::Vector{Float64}, fx::Vector{Float64})
    x_length = length(x)
    y_length = length(fx)

    if (x_length != y_length || x_length == 0)
        throw(DimensionMismatch("Podane wektory nie są tej samej wielkości lub są rozmiaru 0..."))
    end

    n = x_length

    a = zeros(n)
    a[n] = fx[n]
    for i in n-1:-1:1
        a[i] = - (a[i + 1] * x[i]) + fx[i]
        for j in (i+1):(n-1)
            a[j] += -x[i] * a[j+1]
        end
    end

    return a
end

# Funkcja, która interpoluje zadaną funkcję f w przedziale [a, b] za pomocą wielomianu interpolacyjnego stopnia n w postaci Newtona.
# Następnie rysuje wielomian interpolacyjny i interpolowaną funkcję. 
#
# # Argumenty
#
# - `f`: funkcja f(x) zadana jako anonimowa funkcja
# - `a`: początek przedziału interpolacji
# - `b`: koniec przedziału interpolacji
# - `n`: stopień wielomianu interpolacyjnego
#
# # Zwraca
#
# - funkcja rysuje wielomian interpolacyjny i interpolowaną funkcję w przedziale [a, b]
#
function rysujNnfx(f, a::Float64, b::Float64, n::Int)
    # Generowanie węzłów równoodległych
    x = zeros(n + 1)
    fx = zeros(n + 1)
    h = (b - a) / n
    for i in 1:n+1
        x[i] = a + (i - 1) * h
    end
    # Obliczanie wartości funkcji w węzłach
    fx = f.(x)

    # Obliczanie ilorazów różnicowych Newtona
    ilorazy_roznicowe = ilorazyRoznicowe(x, fx)

    # Przygotowanie danych do wykresu
    x_interpolacja = range(a, b, length=1000)
    y_wielomian = [warNewton(x, ilorazy_roznicowe, xi) for xi in x_interpolacja]
    y_funkcja = f.(x_interpolacja)

    # Rysowanie wykresu
    p = plot(size=(800,600))

    plot!(p, x_interpolacja, y_wielomian, label="Wielomian interpolacyjny")
    plot!(p, x_interpolacja, y_funkcja, label="Interpolowana funkcja")
    scatter!(p, x, fx, label="Węzły interpolacji")
    title!("Interpolacja Newtona")
    xlabel!("x")
    ylabel!("y")

    display(p)
    return p
end

end