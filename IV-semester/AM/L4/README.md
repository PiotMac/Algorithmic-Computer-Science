# LISTA 4
## ZAD. 1
Napisz prostą grę w kółko i krzyżyk wykorzystującą Jetpack Compose. Aplikacja powinna mieć możliwość wyboru rozmiaru rozgrywki w zakresie co najmniej od 3x3 do 20x20. Wystarczy sama podstawowa rozgrywka między dwoma użytkownikami reguły też mogą być różne. W tym zadaniu głównym celem jest wykorzystanie Jetpacka do generowania i obsługi UI z kodu. Wykorzystaj też ViewModel do przechowywania i zarządzania danymi związanymi z interfejsem użytkownika w sposób "lifecycle-aware".
## ZAD. 2
Napisz aplikacje (galeria) przechowującą zdjęcia np. ludzi, krajobrazów, zwierząt, ... i każde zdjęcie dodatkowo zawiera krótki opis. Po uruchomieniu aplikacji, na początku pokazuje ona dostępne zdjęcia. Użytkownik może wybrać dowolną pozycję, aby zobaczyć większe zdjęcie i opis. Na ekranie dodatkowo, mamy możliwość ocenienia zdjęcia przez np. "gwiazdki" (zobacz RatingBar ). Proszę pamiętać, że na tym etapie poznania Androida nie ma to być w pełni funkcjonalna aplikacja np. nie potrzeba tworzyć kont dla użytkowników lub nie potrzeba przechowywać nowych zdjęć. Aplikacja powinna natomiast obsługiwać:
 - co najmniej dwie aktywności
 - przekazywać informacje z jednej aktywności do drugiej wykorzystując intencje (tutaj proszę przemyśleć jak to zrobić, dane wysyłane w intencji mają ograniczony rozmiar)
 - druga aktywność powinna wracać informacje do pierwszej o liczbie gwiazdek, po czym w pierwszej aktywności obrazki zostają odpowiednio posortowane po liczbie gwiazdek
 - poprawnie obsługiwać cykl życia aktywności tzn. onCreate, onStart, onResume, onPause, onStop, onDestroy, ... (te które są potrzebne)
 - wykorzystywać fragmenty przy zmianie orientacji ekranu
 - zapamiętywać swój stan po zmianie orientacji ekranu
## ZAD. 3 (*)
Zmień powyższą aplikacje na "tab layout" (ViewPager2) zamiast dwóch aktywności.
## ZAD. 4 (*)
Uzupełnij poprzednie zadanie o możliwość robienia zdjęć np. z dostępnych podstawowych bibliotek i dodawania do kolekcji.
