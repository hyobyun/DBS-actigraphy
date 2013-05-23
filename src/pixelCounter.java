import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import javax.imageio.ImageIO;
 
public class pixelCounter {
    public static void main(String[] args) throws IOException {
        String path = "../output/rawpixelmap.png";
        BufferedImage image = ImageIO.read(new File(path));
        int count=0;
        int black   = Color.black.getRGB();
        int w = image.getWidth();
        int h = image.getHeight();
        for(int y = 0; y < h; y++) {
            for(int x = 0; x < w; x++) {
                if(image.getRGB(x, y) == black)   count++;
            }
        }
        System.out.println(count);
    }
}
