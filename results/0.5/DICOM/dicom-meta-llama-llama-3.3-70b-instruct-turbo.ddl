meta dicom {
  type patient_name = string;
  type patient_id = string;
  type patient_birth_date = date;
  type patient_sex = enum { M, F, O, U };
  type patient_age = string;

  type study_instance_uid = string;
  type study_date = date;
  type study_time = time;
  type study_description = string;
  type referring_physician_name = string;
  type study_id = string;
  type accession_number = string;
  type study_status_id = string;

  type series_instance_uid = string;
  type series_number = int;
  type series_description = string;
  type series_date = date;
  type series_time = time;
  type modality = string;
  type series_type = string;

  type sop_instance_uid = string;
  type image_type = string;
  type image_orientation_patient = array float 6;
  type image_position_patient = array float 3;
  type pixel_data = bytes;
  type rows = uint;
  type columns = uint;
  type pixel_spacing = array float 2;
  type slice_thickness = float;
  type samples_per_pixel = uint;
  type photometric_interpretation = string;
  type bits_allocated = uint;
  type bits_stored = uint;
  type high_bit = uint;
  type pixel_representation = uint;
  type planar_configuration = uint;
  type pixel_aspect_ratio = array float 2;

  type frame_of_reference_uid = string;
  type position_reference_indicator = string;

  type report_id = string;
  type report_status = string;
  type report_priority = string;
  type report_type = string;

  type rt_series_instance_uid = string;
  type rt_series_number = int;
  type rt_series_description = string;
  type rt_series_date = date;
  type rt_series_time = time;

  struct patient {
    patient_name: patient_name;
    patient_id: patient_id;
    patient_birth_date: patient_birth_date;
    patient_sex: patient_sex;
    patient_age: patient_age;
  }

  struct study {
    study_instance_uid: study_instance_uid;
    study_date: study_date;
    study_time: study_time;
    study_description: study_description;
    referring_physician_name: referring_physician_name;
    study_id: study_id;
    accession_number: accession_number;
    study_status_id: study_status_id;
  }

  struct series {
    series_instance_uid: series_instance_uid;
    series_number: series_number;
    series_description: series_description;
    series_date: series_date;
    series_time: series_time;
    modality: modality;
    series_type: series_type;
  }

  struct image {
    sop_instance_uid: sop_instance_uid;
    image_type: image_type;
    image_orientation_patient: image_orientation_patient;
    image_position_patient: image_position_patient;
    pixel_data: pixel_data;
    rows: rows;
    columns: columns;
    pixel_spacing: pixel_spacing;
    slice_thickness: slice_thickness;
    samples_per_pixel: samples_per_pixel;
    photometric_interpretation: photometric_interpretation;
    bits_allocated: bits_allocated;
    bits_stored: bits_stored;
    high_bit: high_bit;
    pixel_representation: pixel_representation;
    planar_configuration: planar_configuration;
    pixel_aspect_ratio: pixel_aspect_ratio;
  }

  struct frame_of_reference {
    frame_of_reference_uid: frame_of_reference_uid;
    position_reference_indicator: position_reference_indicator;
  }

  struct report {
    report_id: report_id;
    report_status: report_status;
    report_priority: report_priority;
    report_type: report_type;
  }

  struct rt_series {
    rt_series_instance_uid: rt_series_instance_uid;
    rt_series_number: rt_series_number;
    rt_series_description: rt_series_description;
    rt_series_date: rt_series_date;
    rt_series_time: rt_series_time;
  }

  struct dicom {
    patient: patient;
    study: study;
    series: series;
    image: image;
    frame_of_reference: frame_of_reference;
    report: report;
    rt_series: rt_series;
  }
}