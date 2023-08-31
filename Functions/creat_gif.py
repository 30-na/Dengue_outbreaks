print("world")
import os
from PIL import Image

# Function to open and convert RGB images
def open_and_convert_image(image_path):
    img = Image.open(image_path)
    img = img.convert("P", dither=Image.NONE, palette=Image.ADAPTIVE)
    return img

# Directory path where RGB images are located
directory_path = r"C:\Users\msin2\Desktop\Dengue_model\Figures\2015-16"


# List to store the converted images
converted_images = []

# Loop through each image and convert it
for i in range(1, 13):
    image_path = os.path.join(directory_path, f"{i:02}.jpg")
    img = open_and_convert_image(image_path)
    converted_images.append(img)

# Save the converted images as a GIF
converted_images[0].save(r"C:\Users\msin2\Desktop\Dengue_model\Figures\2015.gif", save_all=True, append_images=converted_images[1:], optimize=False, duration=800, loop=0)
