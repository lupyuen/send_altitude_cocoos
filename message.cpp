//  Library for composing structured Sigfox messages
//  Based on https://github.com/lupyuen/unabiz-arduino/blob/master/Message.cpp
#include <stdint.h>  //  For uint8_t
#include <string.h>
#include "display.h"
#include "message.h"

//  Encode each letter (lowercase only) in 5 bits:
//  0 = End of name/value or can't be encoded.
//  1 = a, 2 = b, ..., 26 = z,
//  27 = 0, 28 = 1, ..., 31 = 4
//  5 to 9 cannot be encoded.

static const uint8_t firstLetter = 1;
static const uint8_t firstDigit = 27;

static void stop(const char *msg) {
  //  Call this function if we need to stop.
  debug("***** ERROR: ", msg); debug_flush();
  for (;;) {}  //  Loop forever since we can't continue
}

static uint8_t encodeLetter(char ch) {
  //  Convert character ch to the 5-bit equivalent.
  //  Convert to lowercase.
  if (ch >= 'A' && ch <= 'Z') ch = ch - 'A' + 'a';
  if (ch >= 'a' && ch <= 'z') return ch - 'a' + firstLetter;
  //  For 1, 2, ... return the digit
  if (ch >= '0' && ch <= '4') return (ch - '0') + 27;
  //  Can't encode.
  return 0;
}

static char decodeLetter(uint8_t code) {
  //  Convert the 5-bit code to a letter.
  if (code == 0) return 0;
  if (code >= firstLetter && code < firstDigit) return code - firstLetter + 'a';
  if (code >= firstDigit) return code - firstDigit + '0';
  return 0;
}

#define MAX_HEX_BYTES 2  //  Convert 16-bit unsigned integers.
#define MAX_HEX_BUFFER_LENGTH (MAX_HEX_BYTES * 2)  //  Convert 16-bit unsigned integers into 4 hex digits.
static char hexBuffer[MAX_HEX_BUFFER_LENGTH + 1];  //  Warning: Will be reused by toHex() for returning result.
static const uint8_t swapHexDigits[MAX_HEX_BUFFER_LENGTH] = { 2, 3, 0, 1 };  //  Hex digit 0 will be swapped to Hex digit 2, 1->3, 2->0, 3->1.

static const char *toHex(uint16_t v) {
  //  Convert the 16-bit unsigned integer to a string of 4 hex digits.
  //  Warning: Returned buffer will be reused by toHex().  Caller should copy the returned buffer immediately.
  const bool prefixByZero = true;
  int length = 0;
  //  Loop for each hex digit.
  for (uint16_t divisor = 4096; divisor >= 1; divisor = divisor / 16) {
      char digit = '0' + (char)(v / divisor);
      if (digit > '9') { digit = digit - 10 - '0' + 'a'; }
      if (digit > '0' || length > 0 || prefixByZero) {
          if (length <= MAX_HEX_BUFFER_LENGTH ) {
              uint8_t pos = swapHexDigits[length];  //  Swap the hex digits due to endianness.
              hexBuffer[pos] = digit;
              length++;
          }
      }
      v = v % divisor;
  }
  //  TODO: Is this needed?
  if (length == 0) { 
    hexBuffer[length] = '0'; 
    length++;
  }
  //  TODO: Is this needed?
  if (length <= MAX_HEX_BUFFER_LENGTH) { hexBuffer[length] = 0; }
  hexBuffer[MAX_HEX_BUFFER_LENGTH] = 0;  //  Terminate in case of overflow.
  return hexBuffer;
}

//  TODO: Move these messages to Flash memory.
static const char *addFieldHeader = "Message.addField: ";
static const char *tooLong = "****ERROR: Message too long, already ";

Message::Message() {
  //  Construct an empty message.
  encodedMessage[0] = 0;
}

void Message::clear() {
  //  Clear the message and set to empty.
  encodedMessage[0] = 0;
}

bool Message::addField(const char *name, int value) {
  //  Add an integer field scaled by 10.  2 bytes.
  debug_print(addFieldHeader); debug_print(name); debug_print("="); debug_println(value);
  int val = value * 10;
  return addIntField(name, val);
}

bool Message::addField(const char *name, float value) {
  //  Add a float field with 1 decimal place.  2 bytes.
  debug_print(addFieldHeader); debug_print(name); debug_print("="); debug_println(value);
  int val = (int) (value * 10.0);
  return addIntField(name, val);
}

bool Message::addIntField(const char *name, int value) {
  //  Add an int field that is already scaled.  2 bytes for name, 2 bytes for value.
  if (strlen(encodedMessage) + (4 * 2) > MAX_ENCODED_MESSAGE_SIZE) {
    debug_print("*** ERROR: "); debug_print(tooLong); debug_print(" "); 
    debug_print((int) (strlen(encodedMessage) / 2)); debug_println(" bytes"); 
    debug_flush();
    return false;
  }
  addName(name);
  addEncodedString(toHex(value));
  return true;
}

bool Message::addField(const char *name, const char *value) {
  //  Add a string field with max 3 chars.  2 bytes for name, 2 bytes for value.
  debug_print(addFieldHeader); debug_print(name); debug_print("="); debug_println(value);
  if (strlen(encodedMessage) + (4 * 2) > MAX_ENCODED_MESSAGE_SIZE) {
    debug_print("*** ERROR: "); debug_print(tooLong); debug_print(" "); 
    debug_print((int) (strlen(encodedMessage) / 2)); debug_println(" bytes"); 
    debug_flush();
    return false;
  }
  addName(name);
  addName(value);
  return true;
}

bool Message::addName(const char *name) {
  //  Add the encoded field name with 3 letters.
  //  1 header bit + 5 bits for each letter, total 16 bits.
  //  TODO: Assert name has 3 letters.
  //  TODO: Assert encodedMessage is less than 12 bytes.
  //  Convert 3 letters to 3 bytes.
  uint8_t hexBuffer[] = {0, 0, 0};
  for (int i = 0; i <= 2 && i <= strlen(name); i++) {
    //  5 bits for each letter.
    char ch = name[i];
    hexBuffer[i] = encodeLetter(ch);
  }
  //  [x000] [0011] [1112] [2222]
  //  [x012] [3401] [2340] [1234]
  unsigned int result =
      (hexBuffer[0] << 10) +
      (hexBuffer[1] << 5) +
      (hexBuffer[2]);
  addEncodedString(toHex(result));
  return true;
}

const char *Message::getEncodedMessage() {
  //  Return the encoded message to be transmitted.
  //  Warning: Returned buffer will be reused.  Caller should copy the returned buffer immediately.
  return encodedMessage;
}

static uint8_t hexDigitToDecimal(char ch) {
  //  Convert 0..9, a..f, A..F to decimal.
  if (ch >= '0' && ch <= '9') return (uint8_t) ch - '0';
  if (ch >= 'a' && ch <= 'z') return (uint8_t) ch - 'a' + 10;
  if (ch >= 'A' && ch <= 'Z') return (uint8_t) ch - 'A' + 10;
  return 0;
}

#define MAX_RESULT_LENGTH 45  //  Allocate space for {"abc":6553.5,"def":6553.5,"ghi":6553.5}
static char result[MAX_RESULT_LENGTH + 1];

static void concatResult(const char *s) {
    //  Append the string to the decoded result.
    strncat(result, s, MAX_RESULT_LENGTH - strlen(result));
    result[MAX_RESULT_LENGTH] = 0;  //  Terminate the response in case of overflow.
}

#define MAX_INT_LENGTH 5
static char buffer[MAX_INT_LENGTH + 1];

static void concatInt(uint16_t l) {
    //  Append the 16-bit unsigned integer to the decoded result. We only append up to 5 digits, since 16 bits will give max 65,535.
    const int size = MAX_INT_LENGTH + 1;
    bool prefixByZero = false;
    int length = 0;
    for (uint16_t divisor = 10000; divisor >= 1; divisor = divisor / 10) {
        char digit = '0' + (char)(l / divisor);
        if (digit > '9') {
            debug_print("(Overflow)");
            return;
        }
        if (digit > '0' || length > 0 || prefixByZero) {
            if (length < size) {
                buffer[length++] = digit;
            }
        }
        l = l % divisor;
    }
    if (length == 0) { buffer[length++] = '0'; };
    if (length < size) buffer[length] = 0;
    buffer[size - 1] = 0;  //  Terminate in case of overflow.
    concatResult(buffer);
}

const char *Message::decodeMessage(const char *msg) {
  //  Decode the encoded message into JSON. Result looks like:
  //  {"abc":6553.5,"def":6553.5,"ghi":6553.5}
  //  Warning: Returned buffer will be reused by decodeMessage().  Caller should copy the returned buffer immediately.
  //  Decode 2 bytes name, 2 bytes float * 10, 2 bytes name, 2 bytes float * 10, 2 bytes name, 2 bytes float * 10
  strcpy(result, "{");
  for (int i = 0; i < strlen(msg); i = i + 8) {
    const char *name = &msg[i];     //  2 bytes = 4 chars
    const char *val = &msg[i + 4];  //  2 bytes = 4 chars
    unsigned long name2 =
      (hexDigitToDecimal(name[2]) << 12) +
      (hexDigitToDecimal(name[3]) << 8) +
      (hexDigitToDecimal(name[0]) << 4) +
      hexDigitToDecimal(name[1]);
    unsigned long val2 =
      (hexDigitToDecimal(val[2]) << 12) +
      (hexDigitToDecimal(val[3]) << 8) +
      (hexDigitToDecimal(val[0]) << 4) +
      hexDigitToDecimal(val[1]);
    if (i > 0) { concatResult(","); }
    concatResult("\"");
    //  Decode name.
    char name3[] = {0, 0, 0, 0};
    for (int j = 0; j < 3; j++) {
      uint8_t code = name2 & 31;
      char ch = decodeLetter(code);
      if (ch > 0) { name3[2 - j] = ch; }
      name2 = name2 >> 5;
    }
    name3[3] = 0;
    concatResult(name3);
    //  Decode value.
    concatResult("\":"); concatInt((uint16_t) (val2 / 10));
    concatResult("."); concatInt((uint16_t) (val2 % 10));
  }
  concatResult("}");
  return result;
}

void Message::addEncodedString(const char *s) {
    //  Append the encoded string to the encoded message.
    strncat(encodedMessage, s, MAX_ENCODED_MESSAGE_SIZE - strlen(encodedMessage));
    encodedMessage[MAX_ENCODED_MESSAGE_SIZE] = 0;  //  Terminate the response in case of overflow.
}

#ifdef NOTUSED
bool Message::send() {
  //  Send the encoded message to SIGFOX.
  const char *msg = getEncodedMessage();
  if (msg.length() == 0) {
    echo("****ERROR: Nothing to send");  //  TODO: Move to Flash.
    return false;
  }
  if (msg.length() > MAX_ENCODED_MESSAGE_SIZE) {
    echo(tooLong + (strlen(encodedMessage) / 2) + " bytes");
    return false;
  }
  if (wisol) return wisol->sendMessage(msg);
  else if (radiocrafts) return radiocrafts->sendMessage(msg);
  return false;
}
#endif  //  NOTUSED
