import cv2
import mediapipe as mp
import time

class poseEstimation:
    def __init__(self, mode=False, upper_body=False, smooth_landmarks=True, detectionCon=0.5, trackingCon=0.5):
        self.mode = mode
        self.upper_body = upper_body
        self.smooth_landmarks = smooth_landmarks
        self.detectionCon = detectionCon
        self.trackingCon = trackingCon

        self.mpPose = mp.solutions.pose
        self.pose = self.mpPose.Pose(static_image_mode=self.mode,
                                     smooth_landmarks=self.smooth_landmarks,
                                     min_detection_confidence=self.detectionCon,
                                     min_tracking_confidence=self.trackingCon)
        self.mpDraw = mp.solutions.drawing_utils

        # Video Capture
        self.cap = cv2.VideoCapture(0)
        self.cap.set(cv2.CAP_PROP_FRAME_WIDTH, 1920)
        self.cap.set(cv2.CAP_PROP_FRAME_HEIGHT, 1080)


        # Drawing Specs
        self.circle_spec = self.mpDraw.DrawingSpec(color=(0, 255, 0), thickness=-1, circle_radius=2)  # Green circles
        self.line_spec = self.mpDraw.DrawingSpec(color=(255, 0, 0), thickness=1)  # Red lines

        self.pTime = 0

    def detect_pose(self, img):
        img_RGB = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
        return self.pose.process(img_RGB)

    def draw_landmarks(self, img, results):
        if results.pose_landmarks:
            self.mpDraw.draw_landmarks(img, results.pose_landmarks, self.mpPose.POSE_CONNECTIONS,
                                       landmark_drawing_spec=self.circle_spec,
                                       connection_drawing_spec=self.line_spec)
            h, w, _ = img.shape
            for id, lm in enumerate(results.pose_landmarks.landmark):
                cx, cy = int(lm.x * w), int(lm.y * h)
                print(id, cx, cy)
            print('-' * 12)


    def display_fps(self, img):
        cTime = time.time()
        fps = 1 / (cTime - self.pTime)
        self.pTime = cTime
        cv2.putText(img, f"FPS: {int(fps)}", (10, 45), cv2.FONT_HERSHEY_SIMPLEX, 1.5, (0, 255, 0), 3)

    def run(self):
        while True:
            success, img = self.cap.read()

            if not success:
                break

            results = self.detect_pose(img)
            self.draw_landmarks(img, results)
            self.display_fps(img)

            cv2.imshow("Webcam", img)

            if cv2.waitKey(1) & 0xFF == ord('q'):
                break

        self.cap.release()
        cv2.destroyAllWindows()

if __name__ == "__main__":
    PoseEst = poseEstimation()
    PoseEst.run()