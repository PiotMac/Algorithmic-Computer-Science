#include "LiquidCrystal_I2C.h"
#include "Wheels.h"
#include <Servo.h>

// piny dla sonaru (HC-SR04)
#define TRIG A2
#define ECHO A3

// pin kontroli serwo (musi być PWM)
#define SERVO 3

Servo serwo;

byte LCDAddress = 0x27; //0x3f; dla niebieskich)
#define BEEPER 13

LiquidCrystal_I2C lcd(LCDAddress, 16, 2);

uint8_t arrowRight[8] =
{
    0b01000,
    0b01100,
    0b00110,
    0b11111,
    0b11111,
    0b00110,
    0b01100,
    0b01000
};

int counter = 0;
// dir: 0 - forward, 1 - backward
int dir = 0;
bool isSafe = true;
bool change = false;
int stage = 0;
bool isLooking = false;

byte chosenDirectionAfterStop;
int theBiggestDistanceInView;

int argIn = 0;
Wheels w;

void setup() {
  pinMode(BEEPER, OUTPUT);
  pinMode(TRIG, OUTPUT);    // TRIG startuje sonar
  pinMode(ECHO, INPUT);     // ECHO odbiera powracający impuls
  
  w.attach(6,7,5,8, 12,11);

  Serial.begin(9600);

  lcd.init();
  lcd.backlight();

  serwo.attach(SERVO);
  serwo.write(90);
  counter = 10;
  chosenDirectionAfterStop = 255;
  theBiggestDistanceInView = 0;
}

void loop() {
  if (!isLooking) {
    checkDistance();
    checkDriving();
  }
}

void checkDriving() {
  if (counter > 0 && isSafe) {
      w.goForward(counter);
      change = false;
  }
  else {
    w.stop();
    if(isLooking) {
      
    }
    else {
      serwo.write(90);
      if (chosenDirectionAfterStop <= 80) {
        w.takeTurnRight(chosenDirectionAfterStop);
      }
      else if (chosenDirectionAfterStop >= 100) {
        w.takeTurnLeft(chosenDirectionAfterStop);
      }
      chosenDirectionAfterStop = 255;
      theBiggestDistanceInView = 0;
      
      delay(100);
      isSafe = true;
    }
  }
}

void checkDistance() {
  unsigned long tot;      // czas powrotu (time-of-travel)
  unsigned int distance;

  digitalWrite(TRIG, HIGH);
  delay(10);
  digitalWrite(TRIG, LOW);
  tot = pulseIn(ECHO, HIGH);

/* prędkość dźwięku = 340m/s => 1 cm w 29 mikrosekund
 * droga tam i z powrotem, zatem:
 */
  distance = tot/58;

  uint8_t barPos = 1;
  
  lcd.clear();

  lcd.setCursor(0, 0);
  lcd.print("Kat:");
  lcd.setCursor(5,0);
  lcd.print(90);

  lcd.setCursor(0, 1);
  lcd.print("Odl:");
  lcd.setCursor(5, 1);
  lcd.print(distance);

  if (distance <= 25) {
    isSafe = false;
    counter = 0;
    w.stop();
    digitalWrite(TRIG, LOW);
    digitalWrite(ECHO, LOW);
    startLookingAround();
  }
  else {
    isSafe = true;
    counter = 10;
  }
}

void startLookingAround() {
  isLooking = true;
  for(byte angle = 0; angle <= 180; angle+= 20) {
    lookAndTellDistance(angle);
    delay(300);
  }
  isLooking = false;
}

void lookAndTellDistance(byte angle) {
  
  unsigned long tot;      // czas powrotu (time-of-travel)
  unsigned int distance;

  uint8_t barPos = 1;
  
  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("Kat:");
  lcd.setCursor(5,0);
  lcd.print(angle);

  serwo.write(angle);
  
/* uruchamia sonar (puls 10 ms na `TRIGGER')
 * oczekuje na powrotny sygnał i aktualizuje
 */
  digitalWrite(TRIG, HIGH);
  delay(10);
  digitalWrite(TRIG, LOW);
  tot = pulseIn(ECHO, HIGH);

/* prędkość dźwięku = 340m/s => 1 cm w 29 mikrosekund
 * droga tam i z powrotem, zatem:
 */
  distance = tot/58;

  lcd.setCursor(0, 1);
  lcd.print("Odl:");
  lcd.setCursor(5, 1);
  lcd.print(distance);

  if (distance > theBiggestDistanceInView) {
    theBiggestDistanceInView = distance;
    chosenDirectionAfterStop = angle;
  }
}
