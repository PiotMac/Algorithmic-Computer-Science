# LISTA 2
## ZAD. 1
Napisz klasyczną grę Saper (Minesweeper), gdzie wypełniamy siatkę 9x9 polami które na początku są zakryte następnie odkrywamy je w taki sposób aby nie natrafić na minę. Na każdym odkrytym polu dodatkowo wypisywana jest liczba odkrytych min, które stykają się z danym polem. Możemy też zabezpieczyć dane pole flagą. Jeśli gracz odkryje wszystkie pola i zaznaczy flagami wszystkie miny, to wygrywa. Wykorzystaj odpowiedni Layout do rozłożenia przycisków i zrób tak aby przyciski wypełniały odpowiednio ekran oraz czcionka była odpowiednio duża (możesz to przetestować na emulatorze dla różnych rozdzielczości ekranów).
## ZAD. 2
Napisz aplikację "Wisielec", która wyświetla po kolei obrazek wisielca oraz słowo które gracz próbuje zgadnąć. Słowo wybierane jest losowo z dostępnego słownika. Oczywiście cały czas wyświetlane jest słowo z prawidłowo zgadniętymi literami np. dla słowa komputer jeśli gracz zgadł prawidłowo litery o, m i e, słowo będzie wyglądało mniej więcej tak ?om???e?. Do utworzenia obrazków wykorzystaj np. program GIMP nazwij je odpowiednio wisielec0.png, wisielec1.png i tak dalej. Do wyświetlania obrazków wykorzystaj ImageView. Do przechowywania słownika wykorzystaj plik strings.xml.
```
<string-array name="words">
    <item>komputer</item>
    <item>zgadywanka</item>
    ...
</string-array>
```
Słownik możesz wypełnić dowolnymi słowami np. ściągniętymi z internetu przez prosty skrypt.
