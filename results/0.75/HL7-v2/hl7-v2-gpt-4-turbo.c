#include <hammer/hammer.h>
#include <hammer/glue.h>

static HParser *message;
static HParser *segment;
static HParser *field;
static HParser *component;
static HParser *repeat;
static HParser *escape;
static HParser *subcomponent;

static HParsedToken *act_on_segment(const HParseResult *p, void *user_data) {
    // Custom action can be modified as per user's processing requirement
    return H_MAKE_SEQ((HParsedToken **)p->ast->seq->elements, p->ast->seq->used);
}

static HParsedToken *act_on_field(const HParseResult *p, void *user_data) {
    return H_MAKE_SEQ((HParsedToken **)p->ast->seq->elements, p->ast->seq->used);
}

static HParsedToken *act_on_component(const HParseResult *p, void *user_data) {
    return H_MAKE_SEQ((HParsedToken **)p->ast->seq->elements, p->ast->seq->used);
}

static void init_hl7_grammar() {
    H_RULE(whitespace, h_many(h_ch(' ')));
    H_RULE(newline, h_ch('\r'));
    H_ARULE(segment_delim, h_token_str("\r"));
    H_ARULE(field_delim, h_ch('|'));
    H_ARULE(component_delim, h_ch('^'));
    H_ARULE(repeat_delim, h_ch('~'));
    H_ARULE(escape_delim, h_ch('\\'));
    H_ARULE(subcomponent_delim, h_ch('&'));

    escape = h_middle(h_ch('F'), h_ch('.'), h_ch('S')); // Example escape sequence logic
    subcomponent = h_sequence(h_ch('&'), h_many1(h_ch_range(0x20, 0x7E)), NULL);
    subcomponent = h_action(subcomponent, act_on_component, NULL);

    component = h_sequence(component_delim, h_many1(subcomponent), NULL);
    component = h_action(component, act_on_component, NULL);

    repeat = h_sequence(repeat_delim, h_many1(component), NULL);
    repeat = h_action(repeat, act_on_component, NULL);

    field = h_sequence(field_delim, h_many1(repeat), NULL);
    field = h_action(field, act_on_field, NULL);

    segment = h_sequence(h_many1(field), segment_delim, NULL);
    segment = h_action(segment, act_on_segment, NULL);

    message = h_sequence(h_many1(segment), h_end_p(), NULL);
}

int main(int argc, char **argv) {
    init_hl7_grammar();
    HParser *parser = message;
    uint8_t *input;
    size_t length;
    read_file(stdin, &input, &length);
    HParseResult *result = h_parse(parser, input, length);
    if (result) {
        h_pprint(stdout, result->ast, 0, 3);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed!\n");
    }
    free(input);
    return 0;
}