#include <erl_nif.h>
#include <errno.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#include "cantcoap.h"

#define TOKEN_LENGTH 8
#define URI_LENGTH 128

#ifdef __cplusplus
#define BEGIN_C extern "C" {
#define END_C }
#else
#define BEGIN_C
#define END_C
#endif

BEGIN_C

extern int errno;

/* Type, Method, Token, ID, URI */
static ERL_NIF_TERM make_pdu_5(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	int type, method, id;
	char uri[URI_LENGTH + 1];
	unsigned char token[TOKEN_LENGTH + 1];
	ErlNifBinary result;
	CoapPDU *pdu;
	CoapPDU::Type typeenum;
	CoapPDU::Code methodenum;
    
	// unwrap all arguments
    if (argc != 5 || !enif_is_number(env, argv[0]) ||
	                        !enif_is_number(env, argv[1]) ||
	                        !enif_is_list(env, argv[2]) ||
	                        !enif_is_number(env, argv[3]) ||
	                        !enif_is_list(env, argv[4])) {
	    return enif_make_badarg(env);
	}
	if (!enif_get_int(env, argv[0], &type)) {
	    return enif_make_badarg(env);
	}
	if (!enif_get_int(env, argv[1], &method)) {
        return enif_make_badarg(env);
    }
	if (!enif_get_string(env, argv[2], reinterpret_cast<char *>(token), URI_LENGTH, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
	if (!enif_get_int(env, argv[3], &id)) {
        return enif_make_badarg(env);
    }
	if (!enif_get_string(env, argv[4], uri, URI_LENGTH, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
	
	// check enums
	if (type != CoapPDU::COAP_CONFIRMABLE &&
	     type != CoapPDU::COAP_NON_CONFIRMABLE &&
	     type != CoapPDU::COAP_ACKNOWLEDGEMENT &&
	     type != CoapPDU::COAP_RESET) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Invalid type", ERL_NIF_LATIN1));
	}
	if (method != CoapPDU::COAP_GET &&
         method != CoapPDU::COAP_PUT &&
         method != CoapPDU::COAP_POST &&
         method != CoapPDU::COAP_DELETE) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Invalid method", ERL_NIF_LATIN1));
    }
	
	// make the PDU
	typeenum = static_cast<CoapPDU::Type>(type);
	methodenum = static_cast<CoapPDU::Code>(method);
	pdu = new CoapPDU();
	pdu->setVersion(1);
	pdu->setType(typeenum);
	pdu->setCode(methodenum);
	pdu->setToken(token, strlen(reinterpret_cast<char *>(token)));
	pdu->setMessageID(id);
	pdu->setURI(uri, strlen(uri));
	pdu->addOption(CoapPDU::COAP_OPTION_CONTENT_FORMAT,1,(uint8_t*)")");
	
	// return the binary data
	if (!enif_alloc_binary(pdu->getPDULength(), &result)) {
	    delete pdu;
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Unable to allocate the result", ERL_NIF_LATIN1));
	}
	memcpy(result.data, pdu->getPDUPointer(), pdu->getPDULength());
	delete pdu;
	
	return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &result));
}

/* Buffer */
static ERL_NIF_TERM get_content_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary buffer, result;
    CoapPDU *pdu;
    
    // unwrap all arguments
    if (argc != 1 || !enif_is_binary(env, argv[0])) {
        return enif_make_badarg(env);
    }
    if (!enif_inspect_binary(env, argv[0], &buffer)) {
        return enif_make_badarg(env);
    }
    
    // parse the result
    pdu = new CoapPDU(buffer.data, buffer.size);
    if (!pdu->validate()) {
        delete pdu
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Invalid PDU", ERL_NIF_LATIN1));
    }
    
    // return the payload
    if (!enif_alloc_binary(pdu->getPayloadLength(), &result)) {
        delete pdu;
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Unable to allocate the result", ERL_NIF_LATIN1));
    }
    memcpy(result.data, pdu->getPayloadPointer(), pdu->getPayloadLength());
    delete pdu;
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &result));
}

static ErlNifFunc pdu_NIFs[] = {
    {"make_pdu", 5, &make_pdu_5},
    {"get_content", 1, &get_content_1}
};

ERL_NIF_INIT(pdu, pdu_NIFs, NULL, NULL, NULL, NULL);

END_C
