//  Workaround for cross initialisation problem when cocoOS is used in C++ source files.
//  C++ source files should include "cocoos_cpp.h" instead of "cocoos.h".

#include <cocoos.h>  //  Macros will be redefined below.

//  TODO: This overrides OS_MSG_Q_POST macro defined in cocoOS_5.0.1/src/os_msgqueue.h.
//  This is a workaround for the cross initialisation problem when used in C++ code.
//  Caller function must declare the following before task_open():
//    MsgQ_t queue; Evt_t event;  //  TODO: Workaround for msg_post() in C++.
//  The only changes are in these lines: (commented out MsgQ_t and Evt_t)
//    /* MsgQ_t */ queue = os_msgQ_find(task_id);\
//    /* Evt_t */ event = os_msgQ_event_get( queue );\

#define OS_MSG_Q_POST(task_id, msg, delay, period, async )     do {\
                                                                uint8_t os_posted;\
                                                                /* MsgQ_t */ queue = os_msgQ_find(task_id);\
                                                                os_task_set_wait_queue(running_tid, queue);\
                                                                /* Evt_t */ event = os_msgQ_event_get( queue );\
                                                                os_task_set_change_event(running_tid, event);\
                                                                do {\
                                                                    os_posted = os_msg_post( (Msg_t*)&msg, os_msgQ_find(task_id), delay, period );\
                                                                    if ( os_posted == MSG_QUEUE_FULL ){\
                                                                        if ( async == 0 ) {\
                                                                            os_task_set_msg_result(running_tid, os_posted);\
                                                                            event_wait(event);\
                                                                            os_posted = os_task_get_msg_result(running_tid);\
                                                                            event = os_task_get_change_event(running_tid);\
                                                                        }\
                                                                        else {\
                                                                            os_posted = MSG_QUEUE_UNDEF;\
                                                                        }\
                                                                    }\
                                                                } while ( os_posted == MSG_QUEUE_FULL );\
                                                                if ( MSG_QUEUE_POSTED == os_posted ) {\
                                                                	os_signal_event(event);\
                                                                	os_event_set_signaling_tid( event, running_tid );\
                                                                }\
                                                            } while(0)

//  TODO: This overrides OS_MSG_Q_RECEIVE macro defined in cocoOS_5.0.1/src/os_msgqueue.h.
//  This is a workaround for the cross initialisation problem when used in C++ code.
//  Caller function must declare the following before task_open():
//    MsgQ_t queue; Evt_t event;  //  TODO: Workaround for msg_receive() in C++.
//  The only changes are in these lines: (commented out MsgQ_t and Evt_t)
//    /* MsgQ_t */ queue = os_msgQ_find(task_id);\
//    /* Evt_t */ event = os_msgQ_event_get(queue);\

#define OS_MSG_Q_RECEIVE(task_id, pMsg, async)     do {\
                                                    uint8_t os_received;\
                                                    /* MsgQ_t */ queue = os_msgQ_find(task_id);\
                                                    os_task_set_wait_queue(running_tid, queue);\
                                                    /* Evt_t */ event = os_msgQ_event_get(queue);\
                                                    os_task_set_change_event(running_tid, event);\
                                                    do {\
                                                        os_received = os_msg_receive((Msg_t*)pMsg, os_msgQ_find(task_id));\
                                                        if ( os_received == MSG_QUEUE_EMPTY ){\
                                                            if ( async == 0 ) {\
       	                                                        os_task_set_msg_result(running_tid, os_received);\
                                                                event_wait(event);\
                                                                os_received = os_task_get_msg_result(running_tid);\
                                                                event = os_task_get_change_event(running_tid);\
                                                            }\
                                                            else {\
                                                                ((Msg_t*)pMsg)->signal = NO_MSG_ID;\
                                                                os_received = MSG_QUEUE_UNDEF;\
                                                            }\
                                                        }\
                                                    } while ( os_received == MSG_QUEUE_EMPTY );\
                                                    if ( MSG_QUEUE_RECEIVED == os_received) {\
                                                    	os_signal_event(event);\
                                                    	os_event_set_signaling_tid(event, running_tid );\
													}\
                                                } while(0)
