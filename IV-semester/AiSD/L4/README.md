# LISTA 4
## ZAD. 1
Zaimplementuj strukturę BST (Binary Search Tree), przyjmując jak na listach 2. i 3., że
kluczami są liczby całkowite, wraz z następującymi operacjami:
- *insert(k)* - wstawianie nowego wystąpienia klucza *k* do drzewa,
- *delete(k)* - usuwanie jednego wystąpienia klucza *k* w drzewie (jeśli istnieje).
- *height* - funkcja zwracająca bieżącą wysokość drzewa.

Zademonstruj poprawność swojej implementacji w prezentacji w następujących dwóch
przypadkach, dla *n*=50:
- Wstawienie rosnącego ciągu *n* kluczy operacjami *insert*, a następnie usunięcie
losowego ciągu *n* kluczy operacjami *delete*
- Wstawienie losowego ciągu *n* kluczy operacjami *insert*, a następnie usunięcie
losowego ciągu *n* kluczy operacjami *delete*
Załóżmy, że klucze są z przedziału [0, 2n-1]. (Możesz wykorzystać lub przerobić generatory
danych z listy 2.)

W każdym kroku drukuj wykonywaną operację (np. *'insert 48'* albo *'delete 20'*) oraz
stan drzewa po tej operacji, w takiej postaci, aby widoczna była jego struktura, na przykład
tak jak niżej, gdzie 'lewa-prawa strona' zostały wyświetlone jako kierunki 'góra-dół':
```
            /-[48]
          /-[54]
          | \-[257]
        /-[282]
        | \-[310]
      /-[352]
    /-[370]
    | | /-[370]
    | \-[372]
    | \-[484]
  -[517]
    | /-[527]
    | | \-[665]
    | /-[713]
    | /-[725]
    | | \-[736]
    | | \-[788]
    \-[924]
      | /-[939]
      \-[978]
```
## ZAD. 2
Podobnie jak w poprzednich listach zadań, przeprowadź eksperymenty badające złożoność
dla dużych danych (po 20 testów dla wartości n równych: 10 000, 20 000, ..., 100 000), dla
takich samych scenariuszy jak w zadaniu 1. (tj. wstawianie rosnącego ciągu i usuwanie
losowego ciągu oraz wstawianie i usuwanie losowego ciągu) ale bez wyświetlania
wykonywanych operacji i drzew.
Jako miary złożoności zliczaj:
- liczby porównań między kluczami,
- liczby odczytów i podstawień wskaźników łączących elementy struktury drzewa,
- wysokość drzewa po każdej operacji.

Dla każdego *n* zliczaj zarówno średni koszt, jak i maksymalny napotkany koszt pojedynczej
operacji.
Przygotuj obrazy z wykresami uzyskanych wyników dla każdej z tych miar.
## ZAD. 3
Podobnie jak w zadaniu 1, zaimplementuj strukturę RB BST (Red-Black Binary Search
Tree), oraz demonstracje jej działania dla *n*=50.
## ZAD. 4
Wykonaj odpowiednik zadania 2. dla drzew RB BST.
## ZAD. 5
Podobnie jak w zadaniu 1, zaimplementuj Splay Tree, oraz demonstracje jej działania dla *n*=50.
## ZAD. 6
Wykonaj odpowiednik zadania 2. dla drzew Splay Tree.
