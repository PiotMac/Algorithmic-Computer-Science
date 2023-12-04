public enum Predictors {
    INPUT {
        @Override
        public int[] predict(int[] pixel, int[] west, int[] north, int[] northWest, PixelOperations pixelOperations) {
            return pixel;
        }
    },
    W {
        @Override
        public int[] predict(int[] pixel, int[] west, int[] north, int[] northWest, PixelOperations pixelOperations) {
            return west;
        }
    },
    N {
        @Override
        public int[] predict(int[] pixel, int[] west, int[] north, int[] northWest, PixelOperations pixelOperations) {
            return north;
        }
    },
    NW {
        @Override
        public int[] predict(int[] pixel, int[] west, int[] north, int[] northWest, PixelOperations pixelOperations) {
            return northWest;
        }
    },
    N_W_NW {
        @Override
        public int[] predict(int[] pixel, int[] west, int[] north, int[] northWest, PixelOperations pixelOperations) {
            return pixelOperations.subtractPixels(pixelOperations.addPixels(north, west), northWest);
        }
    },
    N_W_NW_2 {
        @Override
        public int[] predict(int[] pixel, int[] west, int[] north, int[] northWest, PixelOperations pixelOperations) {
            int[] result = pixelOperations.subtractPixels(west, northWest);
            result = pixelOperations.scalePixel(result, 2);
            result = pixelOperations.addPixels(north, result);
            return result;
        }
    },
    W_N_NW_2 {
        @Override
        public int[] predict(int[] pixel, int[] west, int[] north, int[] northWest, PixelOperations pixelOperations) {
            int[] result = pixelOperations.subtractPixels(north, northWest);
            result = pixelOperations.scalePixel(result, 2);
            result = pixelOperations.addPixels(west, result);
            return result;
        }
    },
    N_W_2 {
        @Override
        public int[] predict(int[] pixel, int[] west, int[] north, int[] northWest, PixelOperations pixelOperations) {
            int[] result = pixelOperations.addPixels(north, west);
            result = pixelOperations.scalePixel(result, 2);
            return result;
        }
    },
    NEW_STANDARD {
        @Override
        public int[] predict(int[] pixel, int[] west, int[] north, int[] northWest, PixelOperations pixelOperations) {
            int[] result = new int[3];
            for (int i = 0; i < 2; i++) {
                if (northWest[i] >= Math.max(west[i], north[i])) {
                    result[i] = Math.max(west[i], north[i]);
                }
                else if (northWest[i] <= Math.min(west[i], north[i])) {
                    result[i] = Math.min(west[i], north[i]);
                }
                else {
                    result[i] = west[i] + north[i] - northWest[i];
                }
            }
            return result;
        }
    };
    public abstract int[] predict(int[] pixel, int[] west, int[] north, int[] northWest, PixelOperations pixelOperations);
}
