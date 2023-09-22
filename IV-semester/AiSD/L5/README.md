# LISTA 5
## ZAD. 1
Wykorzystująć programowanie dynamiczne, zaimplementuj algorytm wyszukiwania
najdłuższego wspólnego podciągu dwóch ciągów.
Zademonstruj jego działanie dla małych długości ciągów.
Przeprowadź testy złożoności dla ciągów długości n ∈ { 1000, 2000, ... , 5000 } i przygotuj
odpowiednie wykresy z wynikami.
## ZAD. 2
Zaimplementuj kopce złączalne:
- kopiec dwumianowy,
- kopiec Fibonacciego.

Dla każdego rodzaju kopca, dla n ∈ {500, 1000}, wykonaj następujący eksperyment:
  1. Utwórz dwa puste kopce H<sub>1</sub> i H<sub>2</sub>(operacje *Make-Heap*).
  2. Do każdego z tych dwóch kopców wstaw losowy ciąg elementów długości *n* operacjami *Heap-Insert*.
  3. Scal H<sub>1</sub> i H<sub>2</sub> w jeden kopiec H operacją *Heap-Union*.
  4. Na kopcu H wykonaj *2n* operacji *Extract-Min*. (Sprawdzaj, czy ciąg usuwanych elementów jest posortowany i czy dokładnie po ostatniej operacji kopiec staje się
pusty.)

Dla każdej wykonanej operacji policz liczbę wykonanych porównań między kluczami.
Następnie wykonaj "historyczny" wykres przedstawiający liczbę porównań c<sub>i</sub> wykonanych w *i*-tej wykonanej operacji.

Dla danego *n* wykonaj po 5 eksperymentów z wykresami, aby sprawdzić czy występują duże
różnice dla różnych wylosowanych ciągów wejściowych.

Wykonaj także eksperymenty, dla *n* ∈ {100, 200, ⋯, 10000 }, oraz wykres, na którym
przedstawiona jest zależność między n a łączną liczbą porównań we wszystkich operacjach
eksperymentu przy danej wartości *n*.
