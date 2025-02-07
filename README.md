# Swim To Win

## Overview
This project is an infinite animation game built using Assembly language in DOSBox. The game consists of multiple animated objects, keyboard-controlled interactions, a scoring system, and BIOS/DOS services for user input and interface.

## Features
### **Phase I - Objects' Printing**
- **Mountains, Buildings, or Trees:** The top third of the screen will display mountains (or buildings/trees) using a generic function with multiple calls.
- **Ships:** The middle third of the screen will have moving ships of different sizes.
- **Sea:** The bottom third of the screen represents a static sea.
- **Daytime Scene:** The game will initially support daytime mode only.

### **Phase II - Objects' Movements**
- The sky moves **leftward**.
- The ships move **rightward**.

### **Phase III - Keyboard Interrupt**
- **Fish Movement:**
  - Move the fish using arrow keys (left, right, up, down) one cell at a time.
  - Continuous movement when keys are held down.
  - Wrap-around movement when reaching left/right edges.
  - Prevents crossing the sea’s upper boundary.
  - Prevents entering the ships' region.
  - Sound notification when attempting to cross boundaries.
- **Game Exit:**
  - Pressing `ESC` will prompt a confirmation message before exiting.
  - Ensures normal functionality of other DOSBox programs after exit.

### **Phase IV - Timer System**
- **Screen Animation:** The left/right shift movements of the sky and ships occur at regular time intervals.
- **Coin Objects:**
  - Two random objects (green or red) appear in the fish area.
  - If the fish collects:
    - Green coin → **+10 points**
    - Red coin → **+50 points**
  - Coins disappear after:
    - Green → **10 seconds**
    - Red → **5 seconds**
  - A new coin spawns at a random position after each disappears.
- **Score Display:** Shown in the top-right corner.

### **Phase V - BIOS/DOS Services**
- **User Input:**
  - When launching the program, users enter their **name**.
- **Introduction Screen:**
  - Displays welcome message with the user’s name.
  - Shows game instructions (movement, scoring system).
  - Credits: "Developed By: `<Your Name & Roll No>`"
  - **Blinking Prompt:** "Press Enter to Continue or ESC to Exit."
- **Exit Confirmation:**
  - When pressing `ESC` during gameplay: "Are you sure you want to quit? (Y/N)"

### **Bonus - Multitasking (Extra Credit)**
- **Timer Scheduling for:**
  1. Background music
  2. Main animation (mountains & ships movement)

---
## How to Run in DOSBox
```sh
C:\>nasm swim_to_win.asm -o swim_to_win.com
C:\>swim_to_win.com
```

---
## Demo Video
[![Watch the Video](https://img.youtube.com/vi/YOUR_VIDEO_ID_HERE/0.jpg)](https://www.youtube.com/watch?v=YOUR_VIDEO_ID_HERE)

---

## Credits
- Developed by: **ariba-arshad**

