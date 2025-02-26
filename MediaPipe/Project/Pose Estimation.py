import numpy as np
import pandas as pd
import mediapipe as mp
from scipy.io import loadmat
import os
import cv2

class PoseEstimation:
    def __init__(self, mode = True, num_faces = 1, detectionCon = 0.5):
        self.mode = mode
        self.num_faces = num_faces
        self.detectionCon = detectionCon

        self.mp_face_mesh = mp.solutions.face_mesh
        self.face_mesh = self.mp_face_mesh.FaceMesh(static_image_mode=self.mode,
                                          max_num_faces=self.num_faces,
                                          min_detection_confidence=self.detectionCon)
        self.selected_landmarks = [1, 33, 61, 199, 263, 291, 152, 234, 454]
        self.dataset = []

    def load_pose_from_mat(self, mat_path):
        data = loadmat(mat_path)

        if 'Pose_Para' in data:
            pose_para = data['Pose_Para'].flatten()
            print(f"\nFile: {mat_path}")
            return np.degrees(pose_para[0:3])
        else:
            return [np.nan, np.nan, np.nan]

    def extract_and_process_images(self, folder_path):

        data_list = []
        img_files = [f for f in os.listdir(folder_path) if f.endswith(".jpg")]

        for img_name in img_files:
            img_path = os.path.join(folder_path, img_name)
            mat_path = img_path.replace(".jpg", ".mat")

            # Load Roll, Pitch, Yaw
            roll, pitch, yaw = self.load_pose_from_mat(mat_path)

            img = cv2.imread(img_path)
            if img is None:
                continue

            imgRGB = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
            results = self.face_mesh.process(imgRGB)

            if results.multi_face_landmarks:
                for face_landmarks in results.multi_face_landmarks:
                    h, w, _ = img.shape

                    landmarks = []
                    for idx in self.selected_landmarks:
                        lm = face_landmarks.landmark[idx]
                        x, y, z = int(lm.x * w), int(lm.y * h), lm.z
                        landmarks.extend([x, y])

                    # Append row to dataset
                    self.dataset.append([img_name] + landmarks + [roll, pitch, yaw])

        # Save to CSV
        dataset_path = "C:\\Users\\ahmed\\Desktop\\MediaPipe\\Project"
        self.save_dataset(dataset_path)

    def save_dataset(self, folder_path):
        columns = ['image'] + [f'X{i+1}' for i in range(len(self.selected_landmarks))] + [f'Y{i+1}' for i in range(len(self.selected_landmarks))] + ['Roll', 'Pitch', 'Yaw']
        df = pd.DataFrame(self.dataset, columns=columns)
        save_path = os.path.join(folder_path, "landmarks pose dataset.csv")
        df.to_csv(save_path, index=False)
        print(f"\n Dataset saved at successfully")

if __name__ == "__main__":
    PoseEst = PoseEstimation()
    folder_path = "C:\\Users\\ahmed\\Desktop\\MediaPipe\\Project\\AFLW2000"
    PoseEst.extract_and_process_images(folder_path)