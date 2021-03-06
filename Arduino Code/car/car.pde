#define DEBUG 0
#define WAIT_FOR_START 1

unsigned char incomingByte = 0;
unsigned long loop_count = 0;
unsigned char horn = 32;
unsigned char redLED = 64;
unsigned char greenLED = 128;

unsigned char forward = 'a';
unsigned char backward = 2;
unsigned char left = 4;
unsigned char right = 8;

unsigned char PORTB_val;
unsigned char PORTD_val;

unsigned char in_char = 0;

void setup() 
{
  //PORTD = digital IO 0-7
  //horn, redLED, greenLED

  pinMode(5, OUTPUT);      // sets the digital pin as output
  pinMode(6, OUTPUT);      // sets the digital pin as output
  pinMode(7, OUTPUT);      // sets the digital pin as output

  //PORTB = digital IO 8 - 13  
  //right, left, backwards, forward

  pinMode(8, OUTPUT);      // sets the digital pin as output
  pinMode(9, OUTPUT);      // sets the digital pin as output
  pinMode(10, OUTPUT);     // sets the digital pin as output
  pinMode(11, OUTPUT);     // sets the digital pin as output
  pinMode(13, OUTPUT);     // sets the digital pin as output
  
  Serial.begin(9600);      // set up Serial library at 9600 bps
  
  PORTD = redLED;		   // turn on the red LED
  
  flash_led(3,500);
}


void flash_led(unsigned int count, unsigned int rate)
{
  // debug routine that flashes an LED

  int n_count = 0;

  while (n_count < count)
    {
      n_count++;
      digitalWrite(13, HIGH);       // sets the LED on
      delay(rate);                  // waits for a bit
      digitalWrite(13, LOW);        // sets the LED off
      delay(rate);                  // waits for a bit
    }
}

char get_char()
{
  //Function that waits for a character from the serial port
  //If none are received, it returns 0.
  //The timeout is so that if the router stops sending data to the microcontroller,
  //the micrcontroller will stop driving the car, rather than just going forever with
  //the last command.  Timeout is around 250mS.

  while (loop_count < 30000)
    {
      loop_count++;

      if (Serial.available() > 0)
        {
          incomingByte = Serial.read();
          loop_count = 0;
          return incomingByte; 
        }
    }  
  
  loop_count = 0;
  
#if DEBUG
  Serial.print('X', BYTE);
#endif
  
  return 0; 
}

void loop()                       
{  
  //Function that processes input from serial port and drives the car based
  //on that input.

  in_char = get_char();

  if (in_char != 0 ){
    switch (in_char) {
    case 8:
      digitalWrite(8,HIGH);
      break;
    case 4:
      digitalWrite(9,HIGH);
      break;
    case 2:
      digitalWrite(10,HIGH);
      break;
    case 1:
      digitalWrite(11,HIGH);
      break;
    case 32:
      digitalWrite(5,HIGH);
      break;
    }

  }else{
    digitalWrite(8,LOW);
    digitalWrite(9,LOW);
    digitalWrite(10,LOW);
    digitalWrite(11,LOW);
    digitalWrite(5,LOW);
  }
}
