module.exports = {
    networks: {
        testrpc: {
            host: "localhost",
            port: 8546,
            network_id: "*" // Match any network id
        },
        gethtest: {
            host: "localhost",
            port: 8548,
            network_id: 58342
        }
    }
};
