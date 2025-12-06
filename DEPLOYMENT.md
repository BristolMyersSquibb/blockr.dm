# Deploying blockr.dm to Hetzner

Simple deployment using Docker on a Hetzner Cloud server.

## Steps

### 1. Create a server on Hetzner

```bash
hcloud server create --name shiny-blockr --type cx22 --image ubuntu-24.04 --location fsn1
```

Note the IP address from the output.

### 2. Connect to it via SSH

```bash
ssh root@<IP-ADDRESS>
```

### 3. Install Docker on the server

```bash
curl -fsSL https://get.docker.com | sh
```

### 4. Copy app files from your Mac to the server

Run this from your local machine (not the SSH session):

```bash
scp -r /path/to/blockr.dm root@<IP-ADDRESS>:/root/
```

### 5. Build a Docker image

Back in the SSH session:

```bash
cd /root/blockr.dm
docker build -t shiny-blockr .
```

### 6. Run the container

```bash
docker run -d --name shiny-blockr -p 80:3838 --restart always shiny-blockr
```

## Result

App runs at `http://<IP-ADDRESS>`

## Cost

~€4.50/month for CX22 (2 vCPU, 4GB RAM)

## Useful commands

```bash
# Check if container is running
docker ps

# View logs
docker logs shiny-blockr

# Stop the container
docker stop shiny-blockr

# Start it again
docker start shiny-blockr

# Rebuild after changes
docker stop shiny-blockr
docker rm shiny-blockr
docker build -t shiny-blockr .
docker run -d --name shiny-blockr -p 80:3838 --restart always shiny-blockr
```

## Adding HTTPS (optional)

Requires a domain name pointing to the server IP. Use Caddy for automatic HTTPS:

```bash
# Install Caddy
apt install -y caddy

# Stop the container on port 80
docker stop shiny-blockr
docker rm shiny-blockr
docker run -d --name shiny-blockr -p 3838:3838 --restart always shiny-blockr

# Configure Caddy (/etc/caddy/Caddyfile)
yourdomain.com {
    reverse_proxy localhost:3838
}

# Restart Caddy
systemctl restart caddy
```
