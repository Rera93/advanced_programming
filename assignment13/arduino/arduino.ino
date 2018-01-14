#include <LiquidCrystal.h>
#define KEY_COUNT 5

int keyLimits [KEY_COUNT+1] = {50, 190, 380, 555, 790, 1024};
char keyNames [KEY_COUNT+1] [10] = {"Right ", "Up ", "Down " , "Left " , "Select" , "No key"};
LiquidCrystal lcd = LiquidCrystal(8, 9, 4, 5, 6, 7);

boolean isPressed (int button){
  int val = analogRead(A0);
  for (int i = 0; i <= KEY_COUNT; i += 1) {
    if (val < keyLimits[i] && i == button) {
      return true;
    }
  }
  return false;
}

int v0 = 0;

void setup() {
  lcd.begin(16,2);
}


void loop() {
  lcd.setCursor(0, 0);
  if (millis() - 5 > v0) {
    lcd.print("Hello");
    v0 += 5;
  }
}