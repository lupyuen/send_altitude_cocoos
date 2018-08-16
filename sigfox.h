//  Nework definitions for the Sigfox network.
#ifndef SIGFOX_H_
#define SIGFOX_H_

#define MAX_DEVICE_ID_SIZE 8  //  Max number of chars in Sigfox device ID.
#define MAX_DEVICE_CODE_SIZE 16  //  Max number of chars in Sigfox PAC code.
#define MAX_MESSAGE_SIZE 12  //  Sigfox supports up to 12 bytes per message.
#define SEND_DELAY ((unsigned long) 10 * 60 * 1000)  //  According to regulation, messages should be sent only every 10 minutes.

//  Define multiple timeouts for better multitasking.  Uplink to network is slower than normal
//  commands.  Downlink is slowest, up to 1 minute.
//  COMMAND_TIMEOUT < UPLINK_TIMEOUT < DOWNLINK_TIMEOUT
#define COMMAND_TIMEOUT ((unsigned long) 10 * 1000)  //  Wait up to 10 seconds for simple command response from Sigfox module.
#define UPLINK_TIMEOUT ((unsigned long) 20 * 1000)  //  Wait up to 20 seconds for uplink command response from Sigfox module.
#define DOWNLINK_TIMEOUT ((unsigned long) 60 * 1000)  //  Wait up to 60 seconds for downlink command response from Sigfox module.
#define MAX_TIMEOUT (COMMAND_TIMEOUT+UPLINK_TIMEOUT+DOWNLINK_TIMEOUT)  //  Maximum possible timeout. For now we just add up.

//  Define the countries (ISO ALPHA-2 country code) and frequencies that are supported.
//  Based on https://www.sigfox.com/en/coverage, https://www.st.com/content/ccc/resource/technical/document/user_manual/group0/8d/9a/ea/d7/62/06/43/ce/DM00361540/files/DM00361540.pdf/jcr:content/translations/en.DM00361540.pdf
#define RCZ_MASK (3 << 14)  //  Bits 14-15: RCZ
#define RCZ1 (0 << 14)
#define RCZ2 (1 << 14)
#define RCZ3 (2 << 14)
#define RCZ4 (3 << 14)
enum Country {  //  Bits 0-6: First letter. Bits 7-13: Second letter.
  COUNTRY_AR = RCZ4 + 'A'+('R' << 7),  //  Argentina: RCZ4
  COUNTRY_AU = RCZ4 + 'A'+('U' << 7),  //  Australia: RCZ4
  COUNTRY_BE = RCZ1 + 'B'+('E' << 7),  //  Belgium: RCZ1
  COUNTRY_BR = RCZ2 + 'B'+('R' << 7),  //  Brazil: RCZ2
  COUNTRY_CL = RCZ4 + 'C'+('L' << 7),  //  Chile: RCZ4
  COUNTRY_CO = RCZ4 + 'C'+('O' << 7),  //  Colombia: RCZ4
  COUNTRY_CR = RCZ4 + 'C'+('R' << 7),  //  Costa Rica: RCZ4
  COUNTRY_HR = RCZ1 + 'H'+('R' << 7),  //  Croatia: RCZ1
  COUNTRY_CZ = RCZ1 + 'C'+('Z' << 7),  //  Czech Republic: RCZ1
  COUNTRY_DK = RCZ1 + 'D'+('K' << 7),  //  Denmark: RCZ1
  COUNTRY_EC = RCZ4 + 'E'+('C' << 7),  //  Ecuador: RCZ4
  COUNTRY_SV = RCZ4 + 'S'+('V' << 7),  //  El Salvador: RCZ4
  COUNTRY_EE = RCZ1 + 'E'+('E' << 7),  //  Estonia: RCZ1
  COUNTRY_FI = RCZ1 + 'F'+('I' << 7),  //  Finland: RCZ1
  COUNTRY_FR = RCZ1 + 'F'+('R' << 7),  //  France: RCZ1
  COUNTRY_GF = RCZ1 + 'G'+('F' << 7),  //  French Guiana: RCZ1
  COUNTRY_PF = RCZ1 + 'P'+('F' << 7),  //  French Polynesia: RCZ1
  COUNTRY_DE = RCZ1 + 'D'+('E' << 7),  //  Germany: RCZ1
  COUNTRY_GP = RCZ1 + 'G'+('P' << 7),  //  Guadeloupe: RCZ1
  COUNTRY_HK = RCZ4 + 'H'+('K' << 7),  //  Hong Kong: RCZ4
  COUNTRY_HU = RCZ1 + 'H'+('U' << 7),  //  Hungary: RCZ1
  COUNTRY_IR = RCZ1 + 'I'+('R' << 7),  //  Iran: RCZ1
  COUNTRY_IE = RCZ1 + 'I'+('E' << 7),  //  Ireland: RCZ1
  COUNTRY_IT = RCZ1 + 'I'+('T' << 7),  //  Italy: RCZ1
  COUNTRY_JP = RCZ3 + 'J'+('P' << 7),  //  Japan: RCZ3
  COUNTRY_LU = RCZ1 + 'L'+('U' << 7),  //  Luxembourg: RCZ1
  COUNTRY_MY = RCZ4 + 'M'+('Y' << 7),  //  Malaysia: RCZ4
  COUNTRY_MT = RCZ1 + 'M'+('T' << 7),  //  Malta: RCZ1
  COUNTRY_MQ = RCZ1 + 'M'+('Q' << 7),  //  Martinique: RCZ1
  COUNTRY_MX = RCZ2 + 'M'+('X' << 7),  //  Mexico: RCZ2
  COUNTRY_NL = RCZ1 + 'N'+('L' << 7),  //  Netherlands: RCZ1
  COUNTRY_NC = RCZ1 + 'N'+('C' << 7),  //  New Caledonia: RCZ1
  COUNTRY_NZ = RCZ4 + 'N'+('Z' << 7),  //  New Zealand: RCZ4
  COUNTRY_OM = RCZ1 + 'O'+('M' << 7),  //  Oman: RCZ1
  COUNTRY_PA = RCZ4 + 'P'+('A' << 7),  //  Panama: RCZ4
  COUNTRY_PT = RCZ1 + 'P'+('T' << 7),  //  Portugal: RCZ1
  COUNTRY_RE = RCZ1 + 'R'+('E' << 7),  //  Réunion: RCZ1
  COUNTRY_SG = RCZ4 + 'S'+('G' << 7),  //  Singapore: RCZ4
  COUNTRY_SK = RCZ1 + 'S'+('K' << 7),  //  Slovakia: RCZ1
  COUNTRY_ZA = RCZ1 + 'Z'+('A' << 7),  //  South Africa: RCZ1
  COUNTRY_KR = RCZ3 + 'K'+('R' << 7),  //  South Korea: RCZ3
  COUNTRY_ES = RCZ1 + 'E'+('S' << 7),  //  Spain: RCZ1
  COUNTRY_SE = RCZ1 + 'S'+('E' << 7),  //  Sweden: RCZ1
  COUNTRY_CH = RCZ1 + 'C'+('H' << 7),  //  Switzerland: RCZ1
  COUNTRY_TW = RCZ4 + 'T'+('W' << 7),  //  Taiwan: RCZ4
  COUNTRY_TH = RCZ4 + 'T'+('H' << 7),  //  Thailand: RCZ4
  COUNTRY_TN = RCZ1 + 'T'+('N' << 7),  //  Tunisia: RCZ1
  COUNTRY_GB = RCZ1 + 'G'+('B' << 7),  //  United Kingdom: RCZ1
  COUNTRY_AE = RCZ1 + 'A'+('E' << 7),  //  United Arab Emirates: RCZ1
  COUNTRY_US = RCZ2 + 'U'+('S' << 7),  //  United States of America: RCZ2
};

#endif  //  SIGFOX_H_
