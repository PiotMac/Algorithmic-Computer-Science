#!/bin/bash

touch cytat.txt
touch url.txt
touch obrazek.jpg

curl -s https://api.thecatapi.com/v1/images/search | jq -r .[].url > url.txt
curl -s $(cat url.txt) > obrazek.jpg
catimg obrazek.jpg

curl -s https://api.chucknorris.io/jokes/random | jq -r .value > cytat.txt
cat cytat.txt

rm cytat.txt
rm url.txt
rm obrazek.jpg
