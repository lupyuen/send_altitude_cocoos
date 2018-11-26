//  Library for composing structured Sigfox messages
//  Based on https://github.com/lupyuen/unabiz-arduino/blob/master/Message.h
#ifndef MESSAGE_H_
#define MESSAGE_H_
#include "sigfox.h"   //  For MAX_MESSAGE_SIZE
#define MAX_ENCODED_MESSAGE_SIZE (MAX_MESSAGE_SIZE * 2)  //  1 byte = 2 chars when encoded.

#ifdef __cplusplus  //  Message class for C++ only
class Message
{
public:
  Message();     //  Construct an empty message.
  void clear();  //  Clear the message and set to empty.
  bool addField(const char *name, int value);          //  Add an integer field scaled by 10.
  bool addField(const char *name, float value);        //  Add a float field with 1 decimal place.
  bool addField(const char *name, const char *value);  //  Add a string field with max 3 chars.
  const char *getEncodedMessage();  //  Return the encoded message to be transmitted.
  const char *decodeMessage(const char *msg);  //  Decode the encoded message.

private:
  bool addIntField(const char *name, int value);  //  Add an integer field already scaled.
  bool addName(const char *name);  //  Encode and add the 3-letter name.
  void addEncodedString(const char *s);   //  Append the encoded string to the encoded message.
  char encodedMessage[MAX_ENCODED_MESSAGE_SIZE + 1];  //  Encoded message.
};
#endif  //  __cplusplus
#endif  // MESSAGE_H_
