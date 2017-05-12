void scalanative_init(void) {
	return;
}

void* scalanative_alloc(sn_rtti_t* rtti, int64_t size) { // TODO rtti really useful?
	void* allocated = malloc(size);
	if (allocated == NULL) {
		exit(-1);
	}

	return allocated;
}

void* scalanative_field(char* obj, sn_rtti_t* rtti, int32_t id) {
	return obj + rtti->layout.offsets[id];
}

void* scalanative_method_virtual(sn_rtti_t** obj, int32_t id) {
	sn_rtti_t* rtti = *obj;
	return *(rtti->vtable[id]);
}

void* scalanative_method_static(void* method) {
	return method; // TODO really? deref?
}

void* scalanative_method_trait(char* obj, void* dispatchTable, int32_t offset) {
	int32_t id = *((*obj)->id)
	return *dispatchTable[offset][id];
}

int8_t is_class(sn_type_t** obj, sn_rtti_t *rtti) {
	if (rtti->range->last - rtti->range->first == 0) {
		return rtti == *obj;
	} else {
		int32_t id = (*obj)->id;
		return (id > rtti->range->first && id < rtti->range->last);
	}
}

int8_t is_trait(sn_type_t** obj, int8_t** classHasTrait, int32_t traitId) {
	int32_t classId = (*obj)->id;
	return classHasTrait[classId][traitId];
}