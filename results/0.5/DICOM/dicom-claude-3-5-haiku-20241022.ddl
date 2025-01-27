module DICOM {
    type UID = String where length(self) >= 1 and length(self) <= 64;
    type Name = String where length(self) >= 1 and length(self) <= 64;
    type Age = Integer where self >= 0 and self <= 120;

    type Patient {
        patientID: UID,
        name: Name,
        age: Age,
        gender: Enum["MALE", "FEMALE", "OTHER"],
        studyInstanceUID: UID
    }

    type DicomImage {
        instanceUID: UID,
        patient: Patient,
        imageType: Enum["CT", "MRI", "X-RAY", "ULTRASOUND"],
        pixelData: Bytes,
        width: Integer where self > 0,
        height: Integer where self > 0
    }

    type DicomStudy {
        studyInstanceUID: UID,
        patient: Patient,
        studyDate: Date,
        modality: Enum["CT", "MRI", "X-RAY", "ULTRASOUND"],
        images: List<DicomImage>
    }

    rule validatePatient(patient: Patient) {
        assert length(patient.patientID) > 0;
        assert length(patient.name) > 0;
    }

    rule validateDicomImage(image: DicomImage) {
        assert image.width * image.height * 3 == length(image.pixelData);
    }
}