#include <Arduino.h>

#include "Wheels.h"
#include "TimerOne.h"
#include "PinChangeInterrupt.h"

#define INTINPUT0 A0
#define INTINPUT1 A1

// piny dla sonaru (HC-SR04)
#define TRIG A2
#define ECHO A3

volatile int cnt0 = 0;
volatile int cnt1 = 0;
volatile int counterLimit = -1;
volatile bool driving = false;
volatile bool safe = true;


#define SET_MOVEMENT(side,f,b) digitalWrite( side[0], f);\
                               digitalWrite( side[1], b)

Wheels::Wheels() 
{ }

void Wheels::attachRight(int pF, int pB, int pS)
{
    pinMode(pF, OUTPUT);
    pinMode(pB, OUTPUT);
    pinMode(pS, OUTPUT);
    this->pinsRight[0] = pF;
    this->pinsRight[1] = pB;
    this->pinsRight[2] = pS;
}


void Wheels::attachLeft(int pF, int pB, int pS)
{
    pinMode(pF, OUTPUT);
    pinMode(pB, OUTPUT);
    pinMode(pS, OUTPUT);
    this->pinsLeft[0] = pF;
    this->pinsLeft[1] = pB;
    this->pinsLeft[2] = pS;
}

void Wheels::setSpeedRight(uint8_t s)
{
    analogWrite(this->pinsRight[2], s);
}

void Wheels::setSpeedLeft(uint8_t s)
{
    analogWrite(this->pinsLeft[2], s);
}

void Wheels::setSpeed(uint8_t s)
{
    setSpeedLeft(s);
    setSpeedRight(s);
}

void Wheels::attach(int pRF, int pRB, int pRS, int pLF, int pLB, int pLS)
{
    this->attachRight(pRF, pRB, pRS);
    this->attachLeft(pLF, pLB, pLS);
    pinMode(INTINPUT0, INPUT);
    pinMode(INTINPUT1, INPUT);

    PCICR  = 0x02;  // włącz pin change interrupt dla 1 grupy (A0..A5)
    PCMSK1 = 0x03;  // włącz przerwanie dla A0, A1
}

void Wheels::forwardLeft() 
{
    SET_MOVEMENT(pinsLeft, HIGH, LOW);
}

void Wheels::forwardRight() 
{
    SET_MOVEMENT(pinsRight, HIGH, LOW);
}

void Wheels::backLeft()
{
    SET_MOVEMENT(pinsLeft, LOW, HIGH);
}

void Wheels::backRight()
{
    SET_MOVEMENT(pinsRight, LOW, HIGH);
}

void Wheels::takeTurnLeft(byte angle) {
  // angle == 180 -> cm = 21
  // angle == 160 -> cm = 16
  // angle == 140 -> cm = 11
  // angle == 120 -> cm = 6
  // angle == 100 -> cm = 1
  // angle == 90 -> cm = 0
  int turn = angle / 20;
  int cm = (turn - 5) * 5 + 1;
  this->setSpeed(140);
  this->forwardRight();
  counterLimit = int(1.2 * 40.0 * float(cm) / 21.0);
  while (cnt1 <= counterLimit) {
      //this->stop();
  }
  this->stop();
}

void Wheels::takeTurnRight(byte angle) {
  // angle == 80 -> cm = 1
  // angle == 60 -> cm = 6
  // angle == 40 -> cm = 11
  // angle == 20 -> cm = 16
  // angle == 0 -> cm = 21
  int turn = angle / 20;
  int cm = (4 - turn) * 5 + 1;
  this->setSpeed(140);
  this->forwardLeft();
  counterLimit = int(1.2 * 40.0 * float(cm) / 21.0);
  while (cnt0 <= counterLimit) {

  }
  this->stop();
}

void Wheels::forward()
{
    this->forwardLeft();
    this->forwardRight();
    driving = true;
}

void Wheels::back()
{   
    this->backLeft();
    this->backRight();
    driving = true;
}

void Wheels::stopLeft()
{
    SET_MOVEMENT(pinsLeft, LOW, LOW);
}

void Wheels::stopRight()
{
    SET_MOVEMENT(pinsRight, LOW, LOW);
}

void Wheels::stop()
{
    this->stopLeft();
    this->stopRight();
    cnt0 = 0;
    cnt1 = 0;
    counterLimit = -1;
    driving = false;
    delay(500);
}

void Wheels::goForward(uint8_t cm)
{
    this->setSpeedLeft(120);
    this->setSpeedRight(120);
    this->forward();
    // counterLimit = int(1.5 * 40.0 * float(cm) / 21.0);
}

void Wheels::goBack(uint8_t cm)
{
    this->setSpeed(120);
    this->back();
    counterLimit = int(1.5 * 40.0 * float(cm) / 21.0);
    while (cnt1 <= counterLimit) {
    
    }
    this->stop();
}

ISR(PCINT1_vect) {
  if( (PINC & (1 << PC0)) ) {
    cnt0++;
  }

 if( (PINC & (1 << PC1)) ) {
    cnt1++;
 }
}
