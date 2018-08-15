#ifndef SIGFOX_H_
#define SIGFOX_H_

#define MAX_DEVICE_ID_SIZE 8  //  Max number of chars in Sigfox device ID.
#define MAX_DEVICE_CODE_SIZE 16  //  Max number of chars in Sigfox PAC code.
#define MAX_MESSAGE_SIZE 12  //  Sigfox supports up to 12 bytes per message.
#define SEND_DELAY ((unsigned long) (10 * 60 * 1000))  //  According to regulation, messages should be sent only every 10 minutes.
#define COMMAND_TIMEOUT ((unsigned long) (60 * 1000))  //  Wait up to 60 seconds for response from Sigfox module.  Includes downlink response.

//  Define the countries (ISO ALPHA-2 country code) and frequencies that are supported.
#define RCZ_MASK (3 << 14)  //  Bits 14-15: RCZ
#define RCZ1 (0 << 14)
#define RCZ2 (1 << 14)
#define RCZ3 (2 << 14)
#define RCZ4 (3 << 14)
enum Country {  //  Bits 0-6: First letter. Bits 7-13: Second letter.
  //  Argentina: RCZ4
  COUNTRY_AU = RCZ4 + 'A'+('U' << 7),  //  Australia: RCZ4
  COUNTRY_BR = 'B'+('R' << 7),  //  Brazil: RCZ2
  //  Columbia: RCZ4
  COUNTRY_FR = 'F'+('R' << 7),  //  France: RCZ1
  //  Hong Kong: RCZ4
  COUNTRY_JP = 'J'+('P' << 7),  //  Japan: RCZ3
  //  Malaysia: RCZ4
  //  Mexico: RCZ2
  COUNTRY_OM = 'O'+('M' << 7),  //  Oman: RCZ1
  COUNTRY_NZ = 'N'+('Z' << 7),  //  New Zealand: RCZ4
  COUNTRY_SA = 'S'+('A' << 7),  //  South Africa: RCZ1
  COUNTRY_SG = 'S'+('G' << 7),  //  Singapore: RCZ4
  COUNTRY_UK = 'U'+('K' << 7),  //  UK: RCZ1
  COUNTRY_US = 'U'+('S' << 7),  //  USA: RCZ2
  COUNTRY_TW = 'T'+('W' << 7),  //  Taiwan: RCZ4
  //  Thailand: RCZ4
  //  New Caledonia: RCZ1
};

#endif  //  SIGFOX_H_
