# Chronikis Quick-Start Docker Image

Since Chronikis installation is still rather involved, we have created a Docker image that has RStan, Chronikis, and Jupyter notebook installed, along with an "Example" subdirectory containing an example notebook, Chronikis model, and data:
 
[docker image](https://cloud.docker.com/u/adobe/repository/docker/adobe/chronikis)
 
To run the notebook:
1. Install [Docker](https://www.docker.com/).
2. From your command line: `docker run –rm -p 8888:8888 adobe/Chronikis:stancon2019`.
3. The above will start up a docker containing containing Chronikis and Jupyter, and print out for you a URL to access Jupyter from your browser. The URL will look something like `http:// 127.0.0.1:8888/?token=someLongTokenString`.
4. Once you’re accessing Jupyter from your browser, click on the `Example` folder. This folder contains the notebook, example data, and an example Chronikis program.
5. In the `Example` folder, click on the notebook `meantemp_model.ipynb`.
 