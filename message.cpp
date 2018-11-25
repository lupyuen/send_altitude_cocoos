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
  encodedMessage.concat(wisol->toHex(value));
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
  uint8_t buffer[] = {0, 0, 0};
  for (int i = 0; i <= 2 && i <= strlen(name); i++) {
    //  5 bits for each letter.
    char ch = name[i];
    buffer[i] = encodeLetter(ch);
  }
  //  [x000] [0011] [1112] [2222]
  //  [x012] [3401] [2340] [1234]
  unsigned int result =
      (buffer[0] << 10) +
      (buffer[1] << 5) +
      (buffer[2]);
  encodedMessage.concat(wisol->toHex(result));
  return true;
}

const char *Message::getEncodedMessage() {
  //  Return the encoded message to be transmitted.
  return encodedMessage;
}

static uint8_t hexDigitToDecimal(char ch) {
  //  Convert 0..9, a..f, A..F to decimal.
  if (ch >= '0' && ch <= '9') return (uint8_t) ch - '0';
  if (ch >= 'a' && ch <= 'z') return (uint8_t) ch - 'a' + 10;
  if (ch >= 'A' && ch <= 'Z') return (uint8_t) ch - 'A' + 10;
  return 0;
}

const char *Message::decodeMessage(const char *msg) {
  //  Decode the encoded message.
  //  2 bytes name, 2 bytes float * 10, 2 bytes name, 2 bytes float * 10, ...
  const char *result = "{";
  for (int i = 0; i < strlen(msg); i = i + 8) {
    const char *name = msg.substring(i, i + 4);
    const char *val = msg.substring(i + 4, i + 8);
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
    if (i > 0) { result.concat(','); }
    result.concat('"');
    //  Decode name.
    char name3[] = {0, 0, 0, 0};
    for (int j = 0; j < 3; j++) {
      uint8_t code = name2 & 31;
      char ch = decodeLetter(code);
      if (ch > 0) name3[2 - j] = ch;
      name2 = name2 >> 5;
    }
    name3[3] = 0;
    result.concat(name3);
    //  Decode value.
    result.concat("\":"); result.concat((int)(val2 / 10));
    result.concat('.'); result.concat((int)(val2 % 10));
  }
  result.concat('}');
  return result;
}

void Message::addEncodedString(const char *s) {
    strncat(encodedMessage, s, MAX_MESSAGE_SIZE - strlen(encodedMessage));
    encodedMessage[MAX_MESSAGE_SIZE] = 0;  //  Terminate the response in case of overflow.
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
