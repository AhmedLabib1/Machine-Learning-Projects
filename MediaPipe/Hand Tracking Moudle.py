import cv2
import mediapipe as mp
import time

class handTracking:
    def __init__(self, mode=False, max_hands=4, detectionCon=0.5, trackingCon=0.5):
        self.mode = mode
        self.max_hands = max_hands
        self.detectionCon = detectionCon
        self.trackingCon = trackingCon
        self.mpHands = mp.solutions.hands
        self.hands = self.mpHands.Hands(static_image_mode=self.mode,
                                        max_num_hands=self.max_hands,
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

    def detect_hands(self, img):
        """Process the image and detect hand landmarks."""
        imgRGB = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
        return self.hands.process(imgRGB)
    
    def draw_landmarks(self, img, results):
        """Draw landmarks and connections on the image."""
        if results.multi_hand_landmarks:
            h, w, _ = img.shape
            for hand_landmarks in results.multi_hand_landmarks:
                for id, lm in enumerate(hand_landmarks.landmark):
                    cx, cy = int(lm.x * w), int(lm.y * h)

                    # if id in  {4, 8, 12, 16, 20}:
                    #     cv2.circle(img, (cx, cy), 10, (0, 0, 255), cv2.FILLED)

                self.mpDraw.draw_landmarks(img, hand_landmarks, self.mpHands.HAND_CONNECTIONS,
                                            landmark_drawing_spec = self.circle_spec,
                                            connection_drawing_spec = self.line_spec)

    def display_fps(self, img):
        """Calculate and display FPS."""
        cTime = time.time()
        fps = 1 / (cTime - self.pTime)
        self.pTime = cTime
        cv2.putText(img, f"FPS: {int(fps)}",(10, 70), cv2.FONT_HERSHEY_PLAIN, 3, (255, 0, 255), 3)

    def run (self):
        """Main loop to capture and process video frames."""

        while True:
            success, img = self.cap.read()
            if not success:
                break

            results = self.detect_hands(img)
            self.draw_landmarks(img, results)
            self.display_fps(img)

            cv2.imshow("Hand Tracking", img)
            
            if cv2.waitKey(1) & 0xFF == ord('q'):
                return
        
        self.cap.release()
        cv2.destroyAllWindows()

if __name__ == "__main__":
    tracker = handTracking()
    tracker.run()