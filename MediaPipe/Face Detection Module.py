import cv2
import mediapipe as mp
import time

class faceDetection:
    def __init__(self, detectionCon = 0.5):
        self.detectionCon = detectionCon

        self.mpFaceDetection = mp.solutions.face_detection
        self.mpDraw = mp.solutions.drawing_utils
        self.FaceDetection = self.mpFaceDetection.FaceDetection(min_detection_confidence = self.detectionCon)
        
        # Video Capture
        self.cap = cv2.VideoCapture(0)
        self.cap.set(cv2.CAP_PROP_FRAME_WIDTH, 1920)
        self.cap.set(cv2.CAP_PROP_FRAME_HEIGHT, 1080)

        self.pTime = 0

    def detect_face(self, img):
        imgRGB = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
        return self.FaceDetection.process(imgRGB)

    def draw_square(self, img, results):
        if results.detections:
            for id, detection in enumerate(results.detections):
                self.mpDraw.draw_detection(img, detection)

    def display_fps(self, img):
        cTime = time.time()
        fps = 1 / max((cTime - self.pTime), 1e-6)  # Prevent division by zero
        self.pTime = cTime

        cv2.putText(img, f'FPS: {int(fps)}', (10, 45), cv2.FONT_HERSHEY_SIMPLEX, 1.5, (255, 0, 255), 3)

    def run(self):
        while True:
            success, img = self.cap.read()
            
            if not success:
                break
            
            results = self.detect_face(img)
            self.draw_square(img, results)
            self.display_fps(img)

            cv2.imshow("Webcam", img)

            if cv2.waitKey(1) & 0xFF == ord('q'):
                break

        self.cap.release()
        cv2.destroyAllWindows()

if __name__ == "__main__":
    faceDete = faceDetection()
    faceDete.run()