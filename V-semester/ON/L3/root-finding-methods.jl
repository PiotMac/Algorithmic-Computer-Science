#=
LISTA 3
ZADANIE 1, 2, 3
Autor: Piotr Maciejończyk, 268425
=#

module RootFindingMethods

# Przeprowadza znajdowanie pierwiastka za pomocą metody bisekcji (metody połowienia).
#
# # Argumenty
#
# - `f`: Otrzymana anonimowa funkcja.
# - `a`: Lewy koniec przedziału.
# - `b`: Prawy koniec przedziału.
# - `delta`: Tolerancja dla szerokości przedziału.
# - `epsilon`: Tolerancja dla wartości funkcji.
#
# # Zwraca
#
# Krotka `(r, v, it, err)` gdzie:
# - `r`: Szacowany pierwiastek.
# - `v`: Wartość funkcji w pierwiastku.
# - `it`: Liczba iteracji.
# - `err`: Kod błędu (0 dla sukcesu, 1 dla błędu związanego z brakiem zmiany znaku na zadanym przedziale).
#
function mbisekcji(f, a::Float64, b::Float64, delta::Float64, epsilon::Float64)
    u = f(a)
    v = f(b)
    e = b - a

    if (sign(u) == sign(v))
        return a, u, 0, 1
    end

    no_of_iterations = 0
    while true
        no_of_iterations += 1
        e /= 2
        c = a + e
        w = f(c)
        if (abs(e) < delta || abs(w) < epsilon)
            return c, w, no_of_iterations, 0
        end
        if (sign(w) != sign(u))
            b = c
            v = w
        else
            a = c
            u = w
        end
    end
end

# Przeprowadza znajdowanie pierwiastka za pomocą metody Newtona.
#
# # Argumenty
#
# - `f`: Otrzymana anonimowa funkcja.
# - `pf`: Pochodna otrzymanej, anonimowej funkcji.
# - `x0`: Początkowe przybliżenie.
# - `delta`: Tolerancja dla zmiany przybliżenia pierwiastka.
# - `epsilon`: Tolerancja dla wartości funkcji.
# - `maxit`: Maksymalna liczba iteracji.
#
# # Zwraca
#
# Krotka `(r, v, it, err)` gdzie:
# - `r`: Szacowany pierwiastek.
# - `v`: Wartość funkcji w pierwiastku.
# - `it`: Liczba iteracji.
# - `err`: Kod błędu (0 dla sukcesu, 1 dla osiągnięcia limitu iteracji, 2 dla pochodnej bliskiej zeru).
#
function mstycznych(f, pf, x0::Float64, delta::Float64, epsilon::Float64, maxit::Int)
    v = f(x0)
    if (abs(v) < epsilon)
        return x0, v, 0, 0
    end

    for k in 1:maxit
        derivative = pf(x0)
        if (abs(derivative) < epsilon)
            return x0, v, k, 2
        end
        x1 = x0 - v / derivative
        v = f(x1)
        if (abs(x1 - x0) < delta || abs(v) < epsilon)
            return x1, v, k, 0
        end
        x0 = x1
    end

    return x0, v, maxit, 1
end

# Przeprowadza znajdowanie pierwiastka za pomocą metody siecznych.
#
# # Argumenty
#
# - `f`: Otrzymana anonimowa funkcja.
# - `x0`: Pierwsze początkowe przybliżenie.
# - `x1`: Drugie początkowe przybliżenie.
# - `delta`: Tolerancja dla zmiany przybliżenia pierwiastka.
# - `epsilon`: Tolerancja dla wartości funkcji.
# - `maxit`: Maksymalna liczba iteracji.
#
# # Zwraca
#
# Krotka `(r, v, it, err)` gdzie:
# - `r`: Szacowany pierwiastek.
# - `v`: Wartość funkcji w pierwiastku.
# - `it`: Liczba iteracji.
# - `err`: Kod błędu (0 dla sukcesu, 1 dla osiągnięcia limitu iteracji).
#
function msiecznych(f, x0::Float64, x1::Float64, delta::Float64, epsilon::Float64, maxit::Int)
    fx0 = f(x0)
    fx1 = f(x1)
    for k in 1:maxit
        if (abs(fx0) > abs(fx1))
            temp = x0
            x0 = x1
            x1 = temp

            temp = fx0
            fx0 = fx1
            fx1 = temp
        end
        s = (x1 - x0) / (fx1 - fx0)
        x1 = x0
        fx1 = fx0
        x0 -= fx0 * s
        fx0 = f(x0)
        if (abs(x1 - x0) < delta || abs(fx0) < epsilon)
            return x0, fx0, k, 0
        end
    end

    return x0, fx0, maxit, 1
end

end
