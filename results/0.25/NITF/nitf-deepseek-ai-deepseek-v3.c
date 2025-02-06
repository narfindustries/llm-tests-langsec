#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *create_nitf_parser() {
    HParser *uint8_parser = h_uint8();
    HParser *uint16_parser = h_uint16();
    HParser *uint32_parser = h_uint32();
    HParser *uint64_parser = h_uint64();
    HParser *int8_parser = h_int8();
    HParser *int16_parser = h_int16();
    HParser *int32_parser = h_int32();
    HParser *int64_parser = h_int64();
    HParser *float32_parser = h_float();
    HParser *float64_parser = h_double();
    HParser *char_parser = h_ch(' ');
    HParser *string_parser = h_token("", 0);

    HParser *fhdr_parser = h_token("NITF", 4);
    HParser *fver_parser = h_token("02.10", 5);
    HParser *clevel_parser = h_token("03", 2);
    HParser *stype_parser = h_token("BF01", 4);
    HParser *ostaid_parser = h_sequence(h_repeat_n(h_uint8(), 10), NULL);
    HParser *fdt_parser = h_sequence(h_repeat_n(h_uint8(), 14), NULL);
    HParser *ftitle_parser = h_sequence(h_repeat_n(h_uint8(), 80), NULL);
    HParser *fl_parser = h_uint64();
    HParser *hl_parser = h_uint64();
    HParser *numi_parser = h_uint16();
    HParser *nums_parser = h_uint16();
    HParser *numt_parser = h_uint16();
    HParser *numdes_parser = h_uint16();
    HParser *udhdl_parser = h_uint16();
    HParser *udhd_parser = h_sequence(h_repeat_n(h_uint8(), h_length_value(udhdl_parser, h_uint8())), NULL);
    HParser *xhdl_parser = h_uint16();
    HParser *xhd_parser = h_sequence(h_repeat_n(h_uint8(), h_length_value(xhdl_parser, h_uint8())), NULL);

    HParser *im_parser = h_token("IM", 2);
    HParser *iid_parser = h_sequence(h_repeat_n(h_uint8(), 10), NULL);
    HParser *idatim_parser = h_sequence(h_repeat_n(h_uint8(), 14), NULL);
    HParser *tgtid_parser = h_sequence(h_repeat_n(h_uint8(), 17), NULL);
    HParser *ilevel_parser = h_uint8();
    HParser *isorce_parser = h_sequence(h_repeat_n(h_uint8(), 42), NULL);
    HParser *nrows_parser = h_uint32();
    HParser *ncols_parser = h_uint32();
    HParser *pvtype_parser = h_token("INT", 3);
    HParser *irep_parser = h_token("MONO", 4);
    HParser *icat_parser = h_token("VIS", 3);
    HParser *abpp_parser = h_uint8();
    HParser *pjust_parser = h_token("R", 1);
    HParser *icords_parser = h_token("G", 1);
    HParser *igeolo_parser = h_sequence(h_repeat_n(h_uint8(), 60), NULL);
    HParser *nicom_parser = h_uint8();
    HParser *icom_parser = h_sequence(h_repeat_n(h_uint8(), h_length_value(nicom_parser, h_uint8())), NULL);
    HParser *udofl_parser = h_uint16();
    HParser *udofl_data_parser = h_sequence(h_repeat_n(h_uint8(), h_length_value(udofl_parser, h_uint8())), NULL);
    HParser *ixshdl_parser = h_uint16();
    HParser *ixshd_parser = h_sequence(h_repeat_n(h_uint8(), h_length_value(ixshdl_parser, h_uint8())), NULL);

    HParser *sh_parser = h_token("SH", 2);
    HParser *sid_parser = h_sequence(h_repeat_n(h_uint8(), 10), NULL);
    HParser *sname_parser = h_sequence(h_repeat_n(h_uint8(), 20), NULL);
    HParser *scolor_parser = h_sequence(h_repeat_n(h_uint8(), 1), NULL);
    HParser *sxshdl_parser = h_uint16();
    HParser *sxshd_parser = h_sequence(h_repeat_n(h_uint8(), h_length_value(sxshdl_parser, h_uint8())), NULL);

    HParser *te_parser = h_token("TE", 2);
    HParser *textid_parser = h_sequence(h_repeat_n(h_uint8(), 7), NULL);
    HParser *txtalvl_parser = h_uint8();
    HParser *txtdt_parser = h_sequence